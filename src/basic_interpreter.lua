-- VerbOx Interpreter (modular + block-aware)

local IncludeFetch = require("src.include_fetch")

local variables = {}
local solve = nil
local complete = true
local operationType = nil
local libraries = {}

-- Helpers
local function createVariable(varName)
    if varName then variables[varName] = 0 end
end

local function printVariable(varName)
    if varName then
        print(variables[varName])
    end
end

local function executeLibraryFunction(line)
    local libName, funcName, arg = line:match("(%w+)%.(%w+)%(([^)]*)%)")
    if libName and funcName and libraries[libName] then
        local func = libraries[libName][funcName]
        if func then func(arg, variables, solve) end
    end
end

local function parseBlock(lines)
    local i = 1
    while i <= #lines do
        local line = lines[i]:match("^%s*(.-)%s*$")

        -- #include
        if line:match("^#include") then
            local libName = line:match("#include%s+(%w+)")
            libraries[libName] = IncludeFetch:load(libName)

        -- type.to.solve(...)
        elseif line:match("^type%.to%.solve") then
            local op = line:match("type%.to%.solve%((%w+)%)")
            if op then
                operationType = op
                print("Operation type set to:", op)
            end

        -- create.variable(...)
        elseif line:match("^create%.variable") then
            local varName = line:match('create%.variable%("([^"]+)"%)')
            createVariable(varName)

        -- print.variable(...)
        elseif line:match("^print%.variable") then
            local varName = line:match('print%.variable%("([^"]+)"%)')
            printVariable(varName)

        -- Library function call
        elseif line:match("^%w+%.%w+%(") then
            executeLibraryFunction(line)

        -- If statement block
        elseif line:match("^if") then
            local conditionVar, conditionValue = line:match("if%s+(%w+)%s*==%s*(%w+)%s*then")
            local blockLines = {}
            local j = i + 1
            while j <= #lines do
                local inner = lines[j]
                if inner:match("^end") then break end
                table.insert(blockLines, inner)
                j = j + 1
            end

            -- Evaluate condition
            if _G[conditionVar] == _G[conditionValue] or variables[conditionVar] == variables[conditionValue] then
                parseBlock(blockLines)
            end
            i = j -- skip the block

        end

        i = i + 1
    end
end

-- Run .hl file
local function runFile(path)
    local file = io.open(path, "r")
    if not file then error("File not found: "..path) end

    local lines = {}
    for line in file:lines() do table.insert(lines, line) end
    file:close()

    parseBlock(lines)
end

-- Example run
runFile("examples/prototype.hl")
runFile("examples/test.hl")
