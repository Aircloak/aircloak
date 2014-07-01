class ExceptionResult < ActiveRecord::Base
  belongs_to :task

  def self.create_from_proto task_id, analyst_id, exception
    er = where(task_id: task_id, analyst_id: analyst_id, stacktrace: exception.stackEntry)
    if er
      if not exception.accumulated_count.blank?
        er.count = er.count + exception.accumulated_count
        er.save
      end
    else
      create(
        task_id: task_id,
        analyst_id: analyst_id,
        stacktrace: exception.stackEntry,
        count: exception.accumulated_count.blank? ?  0 : exception.accumulated_count
      )
    end
  end
end
