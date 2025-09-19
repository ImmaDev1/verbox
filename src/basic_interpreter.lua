-- basic_interpreter.lua
-- Minimal interpreter: handles only print("...")

-- Interpreter state
local variables = {}
local modules = {}
local events = {}

-- Helpers
local function setVariable(name, value) variables[name] = value end
local function getVariable(name) return variables[name] or "" end

-- Event system
local function registerEvent(name, callback) events[name] = callback end
local function triggerEvent(name, ...) if events[name] then events[name](...) end end

-- Parse lines
local function parse(line)
    line = line:match("^%s*(.-)%s*$") -- trim spaces

    -- Print statement
    if line:match("^print") then
        local content = line:match('print%("([^"]*)"%')
        print(content)
    end
end

-- Run file
local function runFile(path)
    local file = io.open(path, "r")
    if not file then error("File not found: "..path) end
    for line in file:lines() do parse(line) end
    file:close()
end

-- Example usage
runFile("examples/test.hl")
