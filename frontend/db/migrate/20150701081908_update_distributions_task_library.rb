class UpdateDistributionsTaskLibrary < ActiveRecord::Migration
  def change
    distributions = TaskLibrary.where(name: "Aircloak.Distributions").first
    distributions.code = <<-EOS.strip_heredoc
          Aircloak.Distributions = {}

          function Aircloak.Distributions.quantize_property(label, value, params)
            params.step = params.step or (params.max - params.min) / 100
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
