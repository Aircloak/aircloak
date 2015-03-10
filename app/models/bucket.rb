require './lib/proto/air/aggregate_results.pb'

class Bucket < ActiveRecord::Base
  belongs_to :result
  has_one :task, through: :results

  def to_bucket_proto
    BucketPB.new(label: label, string: str_answer, accumulated_count: accumulated_count)
  end

  def self.delete_for_task task
    Bucket.joins(:result).where(results: {task_id: task.id}).delete_all
  end
end
