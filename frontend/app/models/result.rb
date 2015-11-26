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

  def self.delete_for_task task, begin_date = nil, end_date = nil
    ExceptionResult.delete_for_task task, begin_date, end_date
    if begin_date.nil? or end_date.nil? then
      Result.where(task_id: task.id).delete_all
    else
      Result.where(task_id: task.id).where(:created_at => begin_date..end_date).delete_all
    end
  end

  # Convert result with buckets to the format appropriate for usage by clients, such as API consumers.
  def to_client_hash max_size
    {
      :published_at => created_at.utc.to_i * 1000 + created_at.utc.usec / 1000,
      :id => id,
      :buckets => buckets(max_size),
      :histograms => histograms(max_size),
      :exceptions => exception_results.map { |exception|
        {
          :id => exception.id,
          :error => exception.stacktrace,
          :count => exception.count
        }
      }
    }
  end

  def buckets max_size
    return [] if buckets_json.to_s.empty?
    return JSON.parse(buckets_json) if buckets_json.size <= max_size
    return [{label: "notice", value: "result too big", \
             count: "result size (#{buckets_json.size} bytes) exceeds limit (#{max_size} bytes)"}]
  rescue
    []
  end

  def histograms max_size
    return [] if histograms_json.to_s.empty?
    return JSON.parse(histograms_json) if histograms_json.size <= max_size
    return []
  rescue
    []
  end
end
