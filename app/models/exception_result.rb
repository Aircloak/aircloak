class ExceptionResult < ActiveRecord::Base
  belongs_to :buckets
  has_one :task, through: :results

  def self.create_from_proto result, exception
    create(
      result: result,
      stacktrace: exception.stackEntry,
      count: exception.accumulated_count.blank? ? 0 : exception.accumulated_count
    )
  end
end
