class ExceptionResult < ActiveRecord::Base
  belongs_to :buckets
  belongs_to :result
  has_one :task, through: :result

  def self.delete_for_task task
    ExceptionResult.joins(:result).where(results: {task_id: task.id}).delete_all
  end

  def self.create_from_json result, exception
    create(
      result_id: result.id,
      stacktrace: exception["error"],
      count: exception["count"]
    )
  end
end
