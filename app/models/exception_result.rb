class ExceptionResult < ActiveRecord::Base
  belongs_to :query

  def self.create_from_proto task_id, analyst_id, index, exception
    er = where(query_id: task_id, analyst_id: analyst_id, index: index, stacktrace: exception.stackEntry)
    if er
      if not exception.joiners_leavers.blank?
        er.count = er.count + exception.joiners_leavers.joiners - exception.joiners_leavers.leavers
        er.save
      end
    else
      create(
        query_id: task_id,
        analyst_id: analyst_id,
        index: index,
        stacktrace: exception.stackEntry,

        count: if exception.joiners_leavers.blank?
          0
        else
          exception.joiners_leavers.joiners - exception.joiners_leavers.leavers
        end
      )
    end
  end
end
