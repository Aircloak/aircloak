class CreateAggregationTaskLibrary < ActiveRecord::Migration
  def change
    TaskLibrary.create(name: "Aircloak.Aggregation", code: <<-EOS.strip_heredoc
            Aircloak.Aggregation = {}

            -- sum estimation by decomposing the property into a series of small terms sum
            function Aircloak.Aggregation.fast_accumulate_property(name, value)
              -- validate supplied parameters
              assert(type(value) == "number" and value == math.floor(value),
                "invalid value supplied to fast accumulate property call (must be integer)")
              local sign = 1
              if value < 0 then
                sign = -1
                value = -value
              end
              report_property("fast_accumulator", name)
              while value > 3 do
                local term = math.floor(math.sqrt(2 * value)) + 1
                report_property(name, sign * term)
                value = value - term
              end
              if value > 0 then
                report_property(name, sign * value)
              end
            end

            -- sum estimation by reporting the CDF values for each property
            function Aircloak.Aggregation.accumulate_property(name, value)
              -- validate supplied parameters
              assert(type(value) == "number" and value == math.floor(value),
                "invalid value supplied to accumulate property call (must be integer)")
              local sign = 1
              if value < 0 then
                sign = -1
                value = -value
              end
              report_property("cdf_accumulator", name)
              while value > 0 do
                report_property(name, sign * value)
                value = value - 1
              end
            end
          EOS
        )
  end
end
