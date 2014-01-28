require './lib/proto/air/aggregate_results.pb'

class Bucket < ActiveRecord::Base
  belongs_to :result
  has_one :query, through: :result

  # string to display the count
  def self.display_count count
    case count
      when 0 then :no_change
      when self.too_big_count then :too_big
      else count
    end
  end

  # the count representing "too_big"
  def self.too_big_count
    2**32 - 1
  end

  def display_name
    "#{label}#{display_name_str_answer}#{display_name_range}"
  end

  def display_result
    "#{accumulated_count} (#{Bucket.display_count joiners}/#{Bucket.display_count leavers})"
  end

  def to_property_proto
    range = PropertyProto::RangeProto.new(min: range_min, max: range_max) if range_min
    joiners_leavers = JoinersLeaversProto.new(joiners: joiners, leavers: leavers) if joiners && leavers
    PropertyProto.new(label: label, string: str_answer, range: range, joiners_leavers: joiners_leavers,
        accumulated_count: accumulated_count)
  end

private
  def display_name_str_answer
    str_answer ? ": #{str_answer}" : ""
  end

  def display_name_range
    range_min ? " [#{range_min},#{range_max}]" : ""
  end
end
