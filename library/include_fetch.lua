-- include_fetch.lua
-- Handles loading external libraries for VerbOx interpreter

local IncludeFetch = {}

-- Table to store loaded libraries
IncludeFetch.loaded = {}

-- Load a library from library/ folder
function IncludeFetch:load(libName)
    if self.loaded[libName] then
        -- Already loaded
        return self.loaded[libName]
    end

    local path = "library/" .. libName .. ".lua"
    local ok, lib = pcall(dofile, path)
    if ok and lib then
        self.loaded[libName] = lib
        print("Library loaded:", libName)
        return lib
    else
        error("Failed to load library: " .. libName)
    end
end

return IncludeFetch
