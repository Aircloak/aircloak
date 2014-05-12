class ExceptionResult < ActiveRecord::Base
  belongs_to :task

  def self.create_from_proto task_id, analyst_id, index, exception
    er = where(task_id: task_id, analyst_id: analyst_id, index: index, stacktrace: exception.stackEntry)
    if er
      if not exception.accumulation_count.blank?
        er.count = er.count + exception.accumulation_count
        er.save
      end
    else
      create(
        task_id: task_id,
        analyst_id: analyst_id,
        index: index,
        stacktrace: exception.stackEntry,
        count: exception.accumulation_count.blank? ?  0 : exception.accumulation_count
      )
    end
  end
end
