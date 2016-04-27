Aircloak.Utils = {}

-- This trickery allows us to use the table argument and still be able
-- to access global table functions via tbl. This is needed for
-- autocompletion, where argument names are presented.
local tbl = table

function Aircloak.Utils.quantize(count, quant)
  return math.floor(count/quant)*quant
end

function Aircloak.Utils.length(table)
  local count = 0
  for _ in pairs(table) do count = count + 1 end
  return count
end

function Aircloak.Utils.sorted_keys(table, order)
  local keys = {}
  for k in pairs(table) do keys[#keys+1] = k end

  if order then
    tbl.sort(keys, function(a,b) return order(table, a, b) end)
  else
    tbl.sort(keys)
  end

  return keys
end

function Aircloak.Utils.sorted_pairs(table, order)
  local keys = Aircloak.Utils.sorted_keys(table, order)
  local i = 0
  return function()
    i = i + 1
    if keys[i] then
      return keys[i], table[keys[i]]
    end
  end
end

function Aircloak.Utils.same_arrays(a, b)
  if #a ~= #b then return false end

  for i = 1, #a do
    for j = 1, #b do
      if a[i] ~= b[j] then return false end
    end
  end

  return true
end
