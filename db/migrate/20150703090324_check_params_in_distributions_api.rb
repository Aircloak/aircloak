class CheckParamsInDistributionsApi < ActiveRecord::Migration
  def change
    distributions = TaskLibrary.where(name: "Aircloak.Distributions").first
    distributions.code = <<-EOS.strip_heredoc
          Aircloak.Distributions = {}

          function Aircloak.Distributions.quantize_property(label, value, params)
            assert(params.max > params.min, "invalid max and min parameters for quantize property call")
            if params.steps then
              params.step = (params.max - params.min) / params.steps
            else
              params.step = params.step or (params.max - params.min) / 100
              assert(params.step <= params.max - params.min and params.step > 0, "invalid step parameter for quantize property call")
            end
            -- report total count property
            report_property("quantized", label)
            -- check to see if value is in range
            if value < params.min or value > params.max then
              return
            end
            -- report in range property
            report_property(label, "["..params.min..","..params.max..","..params.step.."]")
            -- report range CDF properties
            local iter = params.min + params.step
            while iter < params.max do
              if value < iter then
                report_property(label, "<"..iter)
              end
              iter = iter + params.step
            end
          end
        EOS
    distributions.save!
  end
end
