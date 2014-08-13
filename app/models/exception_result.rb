class ExceptionResult < ActiveRecord::Base
  belongs_to :buckets
  belongs_to :result
  has_one :task, through: :results

  def self.delete_for_task task
    ExceptionResult.joins(:result).where(results: {task_id: task.id}).delete_all
  end

  def self.create_from_proto result, exception
    create(
      result_id: result.id,
      stacktrace: exception.stackEntry,
      count: exception.accumulated_count.blank? ? 0 : exception.accumulated_count
    )
  end
end
