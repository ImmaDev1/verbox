-- multiplication_input.lua
-- VerbOx math library: handles "multiple" operation

local M = {}

-- multiple("a x b") → stores result in variables["$answer"]
function M.multiple(str, variables)
    -- parse the string "9 x 8"
    local a, b = str:match("(%d+)%s*x%s*(%d+)")
    if a and b then
        local result = tonumber(a) * tonumber(b)
        -- store result in $answer
        variables["$answer"] = result
        -- mark solve as complete
        _G.solve = _G.complete
    else
        error("Invalid multiplication string: " .. str)
    end
end

return M
