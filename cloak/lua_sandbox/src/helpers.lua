function report_property(label, value)
  value = tostring(value) -- value can be anything, but we only report strings
  local property = {label = label, value = value}
  if ac_properties == nil then
    ac_properties = {}
  end
  ac_properties[label..value] = property -- report each property once
end

-- Iterator over a user table's rows.
-- Usage:
--  for row in user_table(table_name) do
--    process_row(row)
--  end
function user_table(table_name)
  -- get initial batch
  local batch, complete = ac_get_next_batch(table_name, true)
  local index = 1

  local iterator = function ()
        local row = batch[index]
        -- check if we are finished with current batch
        if row == nil then
          if complete then
            -- no more batches remaining
            return nil
          else
            -- get next batch
            batch, complete = ac_get_next_batch(table_name, false)
            index = 1
            row = batch[index]
          end
        end
        index = index + 1 -- advance cursor
        return row
      end

  return iterator
end

-- Helper function for loading the entire user table in memory at once.
function load_user_table(table_name)
  local array = {}
  for row in user_table(table_name) do
    table.insert(array, row)
  end
  return array
end

-- Returns the list of input user tables.
function get_user_tables()
  return ac_external_call("get_user_tables")
end
