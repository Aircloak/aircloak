class CreateTaskLibraries < ActiveRecord::Migration
  def change
    create_table :task_libraries do |t|
      t.string :name
      t.text :code
      t.timestamps
    end

    TaskLibrary.create(name: "Aircloak", code: "Aircloak = {}")

    TaskLibrary.create(
          name: "Aircloak.DateTime",
          code: <<-EOS.strip_heredoc
            Aircloak.DateTime = {}

            local days = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}

            function Aircloak.DateTime.hour_range(time)
              return string.format("%02d:00 - %02d:00", time.hour, time.hour + 1)
            end

            function Aircloak.DateTime.day(time)
              return days[time.wday]
            end

            function Aircloak.DateTime.date(time)
              return string.format("%04d%02d%02d", time.year, time.month, time.day)
            end

            function Aircloak.DateTime.week(time)
              return math.floor(time.yday / 7) + 1
            end
          EOS
        )

    TaskLibrary.create(
          name: "Aircloak.Utils",
          code: <<-EOS.strip_heredoc
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
                tbl.sort(keys, function(a,b) return order(t, a, b) end)
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
                  return keys[i], t[keys[i]]
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
          EOS
        )
  end
end