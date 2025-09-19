-- src/basic_interpreter.lua (secure Luau interpreter)
-- Reworked to remove use of `load`/dynamic code compilation to prevent code injection.

local Interpreter = {}

-- Interpreter state
local globals = {}
local modules = {}
local events = {}

local function setGlobal(name, value) globals[name] = value end
local function getGlobal(name) return globals[name] end

local function registerEvent(name, callback) events[name] = callback end
local function triggerEvent(name, ...) if events[name] then events[name](...) end end

local function trim(s) return s and s:match("^%s*(.-)%s*$") or s end

-- Safe evaluator: tokenizer + recursive-descent parser
-- Supports:
--   - numbers (integers, floats)
--   - strings with "..." or '...' and basic escapes (\n, \", \', \\)
--   - identifiers (variables/functions), dotted access (a.b.c)
--   - function calls: fn(arg1, arg2)
--   - unary -, not
--   - binary arithmetic: + - * / % ^ 
--   - comparisons: == ~= < <= > >=
--   - logical: and, or
-- Does NOT allow arbitrary Lua code, statements, or injected bytecode.

-- Whitelisted helpers available to evaluated expressions:
local builtin = {
    math = math,
    tostring = tostring,
    tonumber = tonumber,
    type = type,
    print = function(...) print(...) end,
    pairs = pairs,
    ipairs = ipairs,
}

-- TOKENIZER
local function make_tokenizer(src)
    local i = 1
    local len = #src

    local function peek()
        return src:sub(i, i)
    end
    local function nextchar()
        local c = src:sub(i, i)
        i = i + 1
        return c
    end
    local function match_pattern(pat)
        local s, e = src:find("^" .. pat, i)
        if s then
            local tok = src:sub(i, e)
            i = e + 1
            return tok
        end
        return nil
    end

    local keywords = {
        ["and"] = true, ["or"] = true, ["not"] = true,
    }

    local tokens = {}
    local function skip_ws()
        while true do
            local c = peek()
            if c == " " or c == "\t" or c == "\n" or c == "\r" then
                nextchar()
            else
                break
            end
        end
    end

    while i <= len do
        skip_ws()
        if i > len then break end
        local c = peek()

        -- number
        local num = match_pattern("%d+%.?%d*")
        if num then
            tokens[#tokens+1] = {type="number", value=tonumber(num)}
        elseif c == '"' or c == "'" then
            -- string
            local quote = nextchar()
            local acc = {}
            while i <= len do
                local ch = nextchar()
                if ch == "\\" then
                    local esc = nextchar()
                    if esc == "n" then acc[#acc+1] = "\n"
                    elseif esc == "r" then acc[#acc+1] = "\r"
                    elseif esc == "t" then acc[#acc+1] = "\t"
                    elseif esc == "\\" then acc[#acc+1] = "\\"
                    elseif esc == '"' then acc[#acc+1] = '"'
                    elseif esc == "'" then acc[#acc+1] = "'" 
                    else acc[#acc+1] = esc end
                elseif ch == quote then
                    break
                else
                    acc[#acc+1] = ch
                end
            end
            tokens[#tokens+1] = {type="string", value = table.concat(acc)}
        else
            -- identifiers / keywords
            local id = match_pattern("[_%a][_%w]*")
            if id then
                if keywords[id] then
                    tokens[#tokens+1] = {type="keyword", value=id}
                else
                    tokens[#tokens+1] = {type="ident", value=id}
                end
            else
                -- operators or punctuation (attempt multi-char first)
                local two = src:sub(i, i+1)
                if two == "==" or two == "~=" or two == "<=" or two == ">=" then
                    tokens[#tokens+1] = {type="op", value=two}
                    i = i + 2
                else
                    local one = nextchar()
                    if one:match("[%+%-%*%/%%^%%%(%)%,%.<>]") or one == "=" then
                        tokens[#tokens+1] = {type="op", value=one}
                    else
                        error("Unexpected character in expression: '" .. one .. "'")
                    end
                end
            end
        end
    end

    tokens[#tokens+1] = {type="eof"}
    local pos = 1
    local function cur() return tokens[pos] end
    local function advance() pos = pos + 1; return tokens[pos-1] end
    local function accept(type_, val)
        local t = cur()
        if t.type == type_ and (not val or t.value == val) then
            return advance()
        end
        return nil
    end
    local function expect(type_, val)
        local t = cur()
        if t.type == type_ and (not val or t.value == val) then
            return advance()
        end
        error("Expected token " .. type_ .. (val and (" '"..val.."'") or "") .. " but got " .. tostring(t.type) .. (t.value and ("("..t.value..")") or ""))
    end

    return {
        cur = cur,
        advance = advance,
        accept = accept,
        expect = expect,
        tokens = tokens,
    }
end

-- EVALUATOR (parser)
local function make_eval(src, env_lookup)
    local T = make_tokenizer(src)

    local function lookup_ident_chain(first)
        -- parse optional dotted chain: a.b.c
        local name = first
        local chain = {name}
        while T.accept("op", ".") do
            local t = T.expect("ident")
            chain[#chain+1] = t.value
        end
        -- resolve chain in var_table/globals/modules/builtins via env_lookup
        local value = env_lookup(chain[1])
        if value == nil then
            -- try builtins
            value = builtin[chain[1]]
        end
        for i = 2, #chain do
            if type(value) ~= "table" then
                error("Attempt to index non-table value '" .. tostring(chain[i-1]) .. "'")
            end
            value = value[chain[i]]
            if value == nil then break end
        end
        return value
    end

    local function parse_primary()
        local c = T.cur()
        if c.type == "number" then
            T.advance()
            return c.value
        elseif c.type == "string" then
            T.advance()
            return c.value
        elseif c.type == "ident" then
            local first = c.value
            T.advance()
            -- dotted access or call
            if T.cur().type == "op" and T.cur().value == "." then
                -- resolve dotted or eventual call
                local value = lookup_ident_chain(first)
                if T.cur().type == "op" and T.cur().value == "(" then
                    -- call the resolved function
                    if type(value) ~= "function" then error("Attempt to call non-function") end
                    -- parse args
                    T.expect("op", "(")
                    local args = {}
                    if not T.accept("op", ")") then
                        repeat
                            args[#args+1] = parse_expression()
                        until not T.accept("op", ",")
                        T.expect("op", ")")
                    end
                    return value(table.unpack(args))
                else
                    return value
                end
            elseif T.cur().type == "op" and T.cur().value == "(" then
                -- function call: ident(...)
                local func = lookup_ident_chain(first)
                if type(func) ~= "function" then error("Attempt to call non-function: " .. tostring(first)) end
                T.expect("op", "(")
                local args = {}
                if not T.accept("op", ")") then
                    repeat
                        args[#args+1] = parse_expression()
                    until not T.accept("op", ",")
                    T.expect("op", ")")
                end
                return func(table.unpack(args))
            else
                -- variable or builtin lookup
                local val = lookup_ident_chain(first)
                if val == nil then
                    -- fallback to globals/builtins via env_lookup for single ident
                    val = env_lookup(first) or builtin[first]
                end
                return val
            end
        elseif c.type == "op" and c.value == "(" then
            T.advance()
            local v = parse_expression()
            T.expect("op", ")")
            return v
        else
            error("Unexpected token in primary: " .. tostring(c.type) .. (c.value and (" '"..tostring(c.value).."'" ) or ""))
        end
    end

    local function parse_unary()
        local c = T.cur()
        if c.type == "op" and c.value == "-" then
            T.advance()
            local v = parse_unary()
            return -v
        elseif c.type == "keyword" and c.value == "not" then
            T.advance()
            local v = parse_unary()
            return not v
        else
            return parse_primary()
        end
    end

    local function parse_power()
        local left = parse_unary()
        while T.cur().type == "op" and T.cur().value == "^" do
            T.advance()
            local right = parse_unary()
            left = left ^ right
        end
        return left
    end

    local function parse_muldiv()
        local left = parse_power()
        while T.cur().type == "op" and (T.cur().value == "*" or T.cur().value == "/" or T.cur().value == "%") do
            local op = T.advance().value
            local right = parse_power()
            if op == "*" then left = left * right
            elseif op == "/" then left = left / right
            elseif op == "%" then left = left % right end
        end
        return left
    end

    local function parse_addsub()
        local left = parse_muldiv()
        while T.cur().type == "op" and (T.cur().value == "+" or T.cur().value == "-") do
            local op = T.advance().value
            local right = parse_muldiv()
            if op == "+" then left = left + right
            else left = left - right end
        end
        return left
    end

    local function parse_comparison()
        local left = parse_addsub()
        while T.cur().type == "op" and (T.cur().value == "==" or T.cur().value == "~=" or T.cur().value == "<" or T.cur().value == "<=" or T.cur().value == ">" or T.cur().value == ">=") do
            local op = T.advance().value
            local right = parse_addsub()
            if op == "==" then left = (left == right)
            elseif op == "~=" then left = (left ~= right)
            elseif op == "<" then left = (left < right)
            elseif op == "<=" then left = (left <= right)
            elseif op == ">" then left = (left > right)
            elseif op == ">=" then left = (left >= right) end
        end
        return left
    end

    local function parse_and()
        local left = parse_comparison()
        while T.cur().type == "keyword" and T.cur().value == "and" do
            T.advance()
            local right = parse_comparison()
            left = left and right
        end
        return left
    end

    local function parse_expression()
        local left = parse_and()
        while T.cur().type == "keyword" and T.cur().value == "or" do
            T.advance()
            local right = parse_and()
            left = left or right
        end
        return left
    end

    return function() 
        local ok, res = pcall(parse_expression)
        if not ok then error(res) end
        if T.cur().type ~= "eof" then
            error("Unexpected trailing tokens in expression")
        end
        return res
    end
end

-- env lookup function used by evaluator: checks var_table, then globals, then modules
local function make_env_lookup(var_table)
    return function(name)
        if var_table and var_table[name] ~= nil then return var_table[name] end
        if globals[name] ~= nil then return globals[name] end
        if modules[name] ~= nil then return modules[name] end
        return nil
    end
end

-- Public eval function: safe expression evaluation without load()
local function eval(expr, var_table)
    if expr == nil then return nil end
    local exprs = tostring(expr)
    -- Basic sanitization: disallow semicolons which could suggest attempted statement chains
    if exprs:find(";") then error("Invalid character ';' in expression") end
    local env_lookup = make_env_lookup(var_table)
    local runner = make_eval(exprs, env_lookup)
    return runner()
end

-- Helper to check if a line is a bare function call like fname(...) or mod.func(...)
local function try_eval_statement(line, var_table)
    -- If line matches pattern of call: starts with ident or ident.chain and '('
    local s = trim(line)
    local first = s:match("^([_%a][_%w%.]*)%s*%(")
    if not first then return false end
    -- Evaluate by constructing a small expression and ensuring it is function-call syntax only
    -- We safely evaluate by parsing as expression (function call is allowed in parser).
    local ok, res = pcall(function() return eval(s, var_table) end)
    if not ok then error("Runtime error in statement: " .. tostring(res)) end
    return true
end

-- Core interpreter: processes lines, uses eval() for expressions (no load)
local function run_lines(lines, i, var_table)
    var_table = var_table or globals
    local n = #lines

    local function parse_assignment(line)
        local name, expr = line:match("^([_%a][_%w]*)%s*=%s*(.+)$")
        if name then
            local v = eval(expr, var_table)
            var_table[name] = v
            return true
        end
        return false
    end

    local function parse_print(line)
        local inner = line:match("^print%s*%((.+)%)%s*$")
        if inner then
            local v = eval(inner, var_table)
            print(v)
            return true
        end
        return false
    end

    local function parse_function_definition(line, idx, target_table)
        local fname, args = line:match("^function%s+([_%a][_%w]*)%s*%((.-)%)%s*$")
        if not fname then return nil end
        args = trim(args)
        local arg_list = {}
        if args ~= "" then
            for a in args:gmatch("([^,]+)") do table.insert(arg_list, trim(a)) end
        end

        local body = {}
        local depth = 1
        local j = idx + 1
        while j <= n do
            local l = trim(lines[j])
            if l:match("^function%s") then depth = depth + 1 end
            if l == "end" then
                depth = depth - 1
                if depth == 0 then break end
            end
            body[#body+1] = lines[j]
            j = j + 1
        end
        if j > n then error("Unterminated function definition for " .. fname) end

        local closure = function(...)
            local call_env = {}
            if target_table then
                for k,v in pairs(target_table) do call_env[k] = v end
            end
            for k,v in pairs(globals) do if call_env[k] == nil then call_env[k] = v end end
            local given = {...}
            for idx_arg, name_arg in ipairs(arg_list) do call_env[name_arg] = given[idx_arg] end
            local k = 1
            while k <= #body do
                k = run_lines(body, k, call_env)
            end
            return call_env._return
        end

        return fname, closure, j + 1
    end

    while i <= n do
        local rawline = lines[i]
        local line = trim(rawline)
        if line == "" or line:sub(1,2) == "--" then
            i = i + 1
        elseif line:match("^print%s*%(") then
            parse_print(line)
            i = i + 1
        elseif line:match("^function%s") then
            local fname, closure, nextidx = parse_function_definition(line, i, var_table)
            if not fname then error("Malformed function at line " .. i .. ": " .. line) end
            var_table[fname] = closure
            i = nextidx
        elseif line:match("^library%s+") then
            local libname = line:match("^library%s+([_%a][_%w]*)%s*$")
            if not libname then error("Malformed library declaration: " .. line) end
            local body = {}
            local j = i + 1
            while j <= n do
                local l = trim(lines[j])
                if l == "end" then break end
                body[#body+1] = lines[j]
                j = j + 1
            end
            if j > n then error("Unterminated library " .. libname) end
            local mod = {}
            local k = 1
            while k <= #body do
                local subline = trim(body[k])
                if subline:match("^function%s") then
                    local fname, closure, nextidx = parse_function_definition(subline, k, mod)
                    if not fname then error("Malformed function in library " .. libname .. " at body index " .. k) end
                    mod[fname] = closure
                    -- advance k to line after matching end of function
                    local depth = 1
                    k = k + 1
                    while k <= #body do
                        local L = trim(body[k])
                        if L:match("^function%s") then depth = depth + 1 end
                        if L == "end" then
                            depth = depth - 1
                            if depth == 0 then
                                k = k + 1
                                break
                            end
                        end
                        k = k + 1
                    end
                else
                    local name, expr = subline:match("^([_%a][_%w]*)%s*=%s*(.+)$")
                    if name then
                        local v = eval(expr, mod)
                        mod[name] = v
                    end
                    k = k + 1
                end
            end
            modules[libname] = mod
            i = j + 1
        elseif line:match("^while%s+") then
            local cond = line:match("^while%s+(.+)%s+do%s*$")
            if not cond then error("Malformed while at line " .. i .. ": " .. line) end
            local body = {}
            local j = i + 1
            local depth = 1
            while j <= n do
                local l = trim(lines[j])
                if l:match("^while%s") then depth = depth + 1 end
                if l == "end" then
                    depth = depth - 1
                    if depth == 0 then break end
                end
                body[#body+1] = lines[j]
                j = j + 1
            end
            if j > n then error("Unterminated while starting at line " .. i) end
            while true do
                local result = eval(cond, var_table)
                if not result then break end
                local k = 1
                while k <= #body do
                    k = run_lines(body, k, var_table)
                end
            end
            i = j + 1
        elseif line:match("^for%s+") then
            local varname, start_expr, finish_expr, step_expr =
                line:match("^for%s+([_%a][_%w]*)%s*=%s*(.-)%s*,%s*(.-)%s*,?%s*(.-)%s*do%s*$")
            if not varname then
                varname, start_expr, finish_expr = line:match("^for%s+([_%a][_%w]*)%s*=%s*(.-)%s*,%s*(.-)%s+do%s*$")
                step_expr = nil
            end
            if not varname then error("Malformed for loop at line " .. i .. ": " .. line) end
            local startv = eval(start_expr, var_table)
            local finishv = eval(finish_expr, var_table)
            local stepv = step_expr and step_expr ~= "" and eval(step_expr, var_table) or 1
            local j = i + 1
            local body = {}
            local depth = 1
            while j <= n do
                local l = trim(lines[j])
                if l:match("^for%s") then depth = depth + 1 end
                if l == "end" then
                    depth = depth - 1
                    if depth == 0 then break end
                end
                body[#body+1] = lines[j]
                j = j + 1
            end
            if j > n then error("Unterminated for starting at line " .. i) end
            local cur = startv
            if stepv == 0 then error("for loop step cannot be 0") end
            local function continue_condition(c, f, s)
                if s > 0 then return c <= f end
                return c >= f
            end
            while continue_condition(cur, finishv, stepv) do
                var_table[varname] = cur
                local k = 1
                while k <= #body do
                    k = run_lines(body, k, var_table)
                end
                cur = cur + stepv
            end
            i = j + 1
        elseif parse_assignment(line) then
            i = i + 1
        else
            if line == "end" then
                return i + 1
            else
                -- try to interpret as statement (e.g., function call)
                local ok, err = pcall(function() 
                    if not try_eval_statement(line, var_table) then
                        error("Unrecognized statement")
                    end
                end)
                if not ok then
                    error("Unrecognized statement at line " .. i .. ": " .. tostring(err))
                end
                i = i + 1
            end
        end
    end

    return n + 1
end

-- Run source string (splits into lines)
local function runSource(source)
    local lines = {}
    for line in source:gmatch("([^\r\n]*)\r?\n?") do
        -- gmatch returns a final empty capture; handle gracefully
        if line == "" and #lines > 0 and (#table.concat(lines, "\n") >= #source) then break end
        table.insert(lines, line)
        if #table.concat(lines, "\n") >= #source then break end
    end
    if #lines == 1 and lines[1] == source then
        lines = {}
        for l in source:gmatch("([^\n\r]+)") do table.insert(lines, l) end
    end
    local i = 1
    while i <= #lines do
        i = run_lines(lines, i, globals)
    end
end

-- Expose API
Interpreter.runSource = runSource
Interpreter.runLines = run_lines
Interpreter.globals = globals
Interpreter.modules = modules
Interpreter.registerEvent = registerEvent
Interpreter.triggerEvent = triggerEvent
Interpreter.eval = eval

return Interpreter
