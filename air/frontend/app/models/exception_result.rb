class ExceptionResult < ActiveRecord::Base
  belongs_to :buckets
  belongs_to :result
  has_one :task, through: :result

  def self.delete_for_task task, begin_date = nil, end_date = nil
    if begin_date.nil? or end_date.nil? then
      ExceptionResult.joins(:result).where(results: {task_id: task.id}).delete_all
    else
      ExceptionResult.joins(:result).where(results: {task_id: task.id}).where(:created_at => begin_date..end_date).delete_all
    end
  end

  def self.create_from_json result, exception
    create(
      result_id: result.id,
      stacktrace: exception["error"],
      count: exception["count"]
    )
  end
end
