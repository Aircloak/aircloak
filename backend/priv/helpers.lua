function report_property(label, string)
  -- We can't easily convert boolean to string from C (lua_tolstring works only
  -- for numbers and strings), so we perform this conversion here.
  if (type(string) == "boolean") then
    string = tostring(string)
  end

  local property = {label = label, value = string}
  if properties == nil then
    properties = {}
  end
  properties[property] = 1
end

function insert_row(table, row)
  assert(type(row) == "table", "Invalid row data")

  local k,v
  for k, v in pairs(row) do
    assert(type(k) == "string", "Invalid column name")
    local value_type = type(v)
    assert(
          value_type == "string" or value_type == "number" or value_type == "boolean",
          "Invalid column value"
        )
  end

  if insert_actions == nil then
    insert_actions = {}
  end

  local t = insert_actions[table]
  if t == nil then
    t = {}
    insert_actions[table] = t
  end

  -- Verify max number of rows per table. We keep an internally used global table
  -- for this case, so we don't need to count rows on every insertion.
  local maxRows = 100
  if t[row] == nil then
    if __ac_inserted_rows == nil then
      __ac_inserted_rows = {}
    end
    __ac_inserted_rows[table] = (__ac_inserted_rows[table] or 0) + 1
    assert(__ac_inserted_rows[table] <= maxRows, "Too many inserted rows. You can insert at most " .. maxRows .. " rows per table.")
  end
  t[row] = 1
end

function lookup(table_name, key)
  return external_call("lookup", table_name, key)
end