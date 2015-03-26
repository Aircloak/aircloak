require './lib/proto/air/aggregate_results.pb'

class Result < ActiveRecord::Base
  belongs_to :task
  belongs_to :analyst
  has_many :exception_results, dependent: :destroy

  # This does an efficient SQL delete, rather than
  # loading all the data, running all the validations
  # and callbacks, etc
  def efficient_delete
    ExceptionResult.where(result_id: self.id).delete_all
    destroy
  end

  def self.delete_for_task task
    ExceptionResult.delete_for_task task
    Result.where(task_id: task.id).delete_all
  end

  # Convert result with buckets to the format appropriate for usage by clients, such as API consumers.
  def to_client_hash
    {
      :published_at => created_at.utc.to_i * 1000 + created_at.utc.usec / 1000,
      :id => id,
      :buckets => buckets,
      :exceptions => exception_results.map { |exception|
        {
          :id => exception.id,
          :count => exception.count
        }
      }
    }
  end

  def buckets
    buckets_json.to_s.empty? ? [] : JSON.parse(buckets_json)
  rescue
    []
  end
end
