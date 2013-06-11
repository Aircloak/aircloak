class PercentileResult < ActiveRecord::Base
  belongs_to :percentile
  has_one :query, through: :percentile

  def self.create_from_proto query_id, perc
    percentile = Percentile.from_proto query_id, perc

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
      raw_values: raw_values,
      percentile: percentile
    )
  end

  def min
    self.raw_values["min"].to_i
  end

  def max
    self.raw_values["max"].to_i
  end
end
