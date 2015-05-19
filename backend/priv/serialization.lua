-- Serialization and deserialization functions. Lua doesn't support native serialization,
-- so we do this hack, based on guide from lua.org (http://www.lua.org/pil/12.1.2.html).
-- The idea is to generate a series of statement which, when eval'd, creates the
-- appropriate data. Serialization works for complex objects (tables), and is also
-- able to detect cycles.

function deserialize(serialized)
  if type(serialized) == "string" then
    -- If it's a string, we just eval it, which returns a function. Then, we
    -- simply execute a function, thus returning the restored value.
    return (loadstring(serialized))()
  else
    return nil
  end
end

function serialize(value)
  if (value == nil) then
    return nil
  else
    -- The code will contain series of variable setters.
    -- var_name will contain the name of the variable that contains the final result.
    var_name, code = serializeRecursive(value, {}, {1})
    return code.."\nreturn "..var_name
  end
end

function serializeRecursive(value, saved, var_name_suffix)
  if saved[value] then    -- value already saved?
    return saved[value], ""
  else
    if type(value) == "table" then
      -- When serializing a table, we generate a series of statement:
      --   local local_N = {}
      --   local_N[key_1] = value_1
      --   ...
      local var_name = "local_"..var_name_suffix[1]
      -- save name for next time
      saved[value] = var_name
      -- increment index to avoid var clashes
      var_name_suffix[1] = var_name_suffix[1] + 1
      local result = "local "..var_name.."={}\n"
      for k,v in pairs(value) do
        local key_name, key_serialized = serializeRecursive(k, saved, var_name_suffix)
        local value_name, value_serialized = serializeRecursive(v, saved, var_name_suffix)
        result = result..key_serialized..value_serialized
        result = result..var_name.."["..key_name.."]="..value_name.."\n"
      end
      return var_name, result
    else
      return serializePrimitiveType(value), ""
    end
  end
end

function serializePrimitiveType(o)
  if type(o) == "number" or type(o) == "boolean" then
    return tostring(o)
  elseif type(o) == "string" then
    -- escapes quotes
    return string.format("%q", o)
  else
    -- Either nil, or unsupported type.
    return "nil"
  end
end