-- VerbOx Interpreter with external libraries

local variables = {}
local solve = nil
local complete = true
local operationType = nil
local libraries = {} -- loaded libraries

-- Load library from library/ folder
local function loadLibrary(line)
    local libName = line:match("#include%s+(%w+)")
    local path = "library/" .. libName .. ".lua"
    local ok, lib = pcall(dofile, path)
    if ok and lib then
        libraries[libName] = lib
        print("Library "..libName.." loaded")
    else
        print("Failed to load library: "..libName)
    end
end

-- Set operation type
local function setOperationType(line)
    local op = line:match("type%.to%.solve%((%w+)%)")
    if op then
        operationType = op
        print("Operation type set to:", op)
    end
end

-- Create variable
local function createVariable(line)
    local varName = line:match('create%.variable%("([^"]+)"%)')
    if varName then
        variables[varName] = 0
    end
end

-- Print variable
local function printVariable(line)
    local varName = line:match('print%.variable%("([^"]+)"%)')
    if varName then
        print(variables[varName])
    end
end

-- Execute library function: e.g., math.multiple("9 x 8")
local function executeLibraryFunction(line)
    local libName, funcName, arg = line:match("(%w+)%.(%w+)%(([^)]*)%)")
    if libName and funcName and libraries[libName] then
        local func = libraries[libName][funcName]
        if func then func(arg, variables) end
    end
end

-- Parse a single line
local function parse(line)
    line = line:match("^%s*(.-)%s*$")

    if line:match("^#include") then
        loadLibrary(line)
    elseif line:match("^type%.to%.solve") then
        setOperationType(line)
    elseif line:match("^create%.variable") then
        createVariable(line)
    elseif line:match("^print%.variable") then
        printVariable(line)
    elseif line:match("^if") then
        if solve == complete then
            print("Condition true, executing block...")
        end
    elseif line:match("^%w+%.%w+%(") then
        executeLibraryFunction(line)
    end
end

-- Run .hl file
local function runFile(path)
    local file = io.open(path, "r")
    if not file then error("File not found: "..path) end
    for line in file:lines() do parse(line) end
    file:close()
end

-- Example run
runFile("examples/test.hl")
