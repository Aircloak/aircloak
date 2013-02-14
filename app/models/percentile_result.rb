class PercentileResult < ActiveRecord::Base
  belongs_to :query

  def self.create_from_proto query_id, perc
    raw_values = {
      min: perc.min,
      max: perc.max
    }
    perc.points.each do |point|
      raw_values[point.value] = {
        percent: point.percent,
        noise: point.percent_noise
      }
    end
    create(
      bucket: perc.name,
      raw_values: raw_values

      # TODO: Also add analyst here
      query_id: query_id
    )
  end

  def min
    self.raw_values["min"].to_i
  end

  def max
    self.raw_values["max"].to_i
  end
end
