class Bucket < ActiveRecord::Base
  belongs_to :result
  has_one :query, through: :result

  # string to display the score
  def self.display_score score
    case score
      when 0 then :no_change
      when self.too_big_score then :too_big
      else score
    end
  end

  # the score representing "too_big"
  def self.too_big_score
    1024*1024*1024*4-1  # TODO: lookup how to make 2^something in Ruby
  end

  def display_name
    "#{label}#{display_name_str_answer}#{display_name_range}"
  end

  def display_result
    "#{accumulated_count} (#{Bucket.display_score joiners}/#{Bucket.display_score leavers})"
  end

private
  def display_name_str_answer
    if str_answer
      ": #{str_answer}"
    else
      ""
    end
  end

  def display_name_range
    if range_min
      " [#{range_min},#{range_max}]"
    else
      ""
    end
  end
end
