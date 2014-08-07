require './lib/proto/air/aggregate_results.pb'

class Bucket < ActiveRecord::Base
  belongs_to :result
  has_one :task, through: :results

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
    "#{label}#{display_name_str_answer}"
  end

  def display_result
    accumulated_count
  end

  def to_bucket_proto
    BucketPB.new(label: label, string: str_answer, accumulated_count: accumulated_count)
  end

  def self.delete_for_task task
    Bucket.joins(:result).where(results: {task_id: task.id}).delete_all
  end

private
  def display_name_str_answer
    str_answer ? ": #{str_answer}" : ""
  end
end
