-- Interpreter for VerbOx
local modules = {}
local variables = {}
local operationType = nil
local solved = nil
local complete = true -- for conditional simulation

-- Load a module
local function loadModule(name)
    local success, mod = pcall(require, ("modules.%s"):format(name))
    if success then
        modules[name] = mod
        print("Module "..name.." imported")
    else
        warn("Failed to load module:", name)
    end
end

-- Parse and execute a single line
local function parse(line)
    line = line:match("^%s*(.-)%s*$") -- trim spaces

    if line:match("^#include") then
        local modName = line:match("#include%s+(%w+)")
        loadModule(modName)

    elseif line:match("^assign type") then
        operationType = line:match("assign type%((%w+)%)")
        print("Operation type assigned:", operationType)

    elseif line:match("^math%.multiple") then
        local arg = line:match('math%.multiple%("([^"]+)"%)')
        modules["math"].multiple(arg, variables)
        solved = complete

    elseif line:match("^if") then
        if solved == complete then
            print("Condition true, executing block...")
        end

    elseif line:match("^create%.variable") then
        local varName = line:match('create%.variable%("([^"]+)"%)')
        variables[varName] = variables[varName] or 0

    elseif line:match("^print") then
        local varName = line:match('print%("([^"]+)"%)')
        print(variables[varName] or varName)
    end
end

-- Run a VerbOx file
local function runFile(path)
    local file = io.open(path, "r")
    if not file then
        error("File not found: "..path)
    end

    for line in file:lines() do
        parse(line)
    end

    file:close()
end

-- Example run
local testFile = "examples/multiplication.hl"
runFile(testFile)
