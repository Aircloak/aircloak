class ExceptionResult < ActiveRecord::Base
  belongs_to :buckets
  has_one :task, through: :results

  def self.create_from_proto result, exception
    create(
      result_id: result.id,
      stacktrace: exception.stackEntry,
      count: exception.accumulated_count.blank? ? 0 : exception.accumulated_count
    )
  end
end
