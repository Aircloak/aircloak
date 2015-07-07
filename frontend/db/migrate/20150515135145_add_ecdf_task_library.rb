class AddEcdfTaskLibrary < ActiveRecord::Migration
  def change
    TaskLibrary.create(
          name: "Aircloak.Distributions",
          code: <<-EOS.strip_heredoc
            Aircloak.Distributions = {}

            -- Function to report ECDF from a distribution.
            -- The implementation is still somewhat experimental,
            -- and will be updated as we get a better understanding
            -- of best practises with regard to reporting ecdf's.
            function Aircloak.Distributions.ecdf(value, title, legend, x_label, y_label)
              title = title or "ECDF"
              x_label = x_label or "Value"
              y_label = y_label or "% of users"
              legend = "ECDF of " .. x_label

              report_property("ac_postprocessing", "ecdf")
              report_property("ac_ecdf_title", title)
              report_property("ac_ecdf_x_label", x_label)
              report_property("ac_ecdf_y_label", y_label)
              report_property("ac_ecdf_legend", legend)

              -- We report all values LESS THAN AND INCLUDING the actual value
              -- this way, we get an inverted ECDF, that in post processing
              -- needs to be flipped so it becomes a regular ECDF.
              for i = 0, value do
                report_property("ac_ecdf_val_less_or_equal", i)
              end
            end
          EOS
        )
  end
end
