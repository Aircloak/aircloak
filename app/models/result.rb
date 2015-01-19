require './lib/proto/air/aggregate_results.pb'

class Result < ActiveRecord::Base
  belongs_to :task
  belongs_to :analyst
  has_many :buckets, dependent: :destroy
  has_many :exception_results, dependent: :destroy

  # This does an efficient SQL delete, rather than
  # loading all the data, running all the validations
  # and callbacks, etc
  def efficient_delete
    Bucket.where(result_id: self.id).delete_all
    ExceptionResult.where(result_id: self.id).delete_all
    destroy
  end

  def self.delete_for_task task
    Bucket.delete_for_task task
    ExceptionResult.delete_for_task task
    Result.where(task_id: task.id).delete_all
  end

  def to_result_proto
    bs = buckets.map(&:to_bucket_proto)
    ResultPB.new(analyst_id: task.analyst_id, task_id: task.id, index: task.index, buckets: bs, exceptions: [])
  end

  def to_client
    {
      :published_at => created_at.utc.to_i * 1000,
      :id => id,
      :buckets => buckets.map { |bucket|
        {
          :name => bucket.display_name,
          :value => bucket.display_result
        }
      },
      :exceptions => exception_results.map { |exception|
        {
          :id => exception.id,
          :count => exception.count
        }
      }
    }
  end
end
