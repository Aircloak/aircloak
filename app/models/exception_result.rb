class ExceptionResult < ActiveRecord::Base
  belongs_to :query

  def self.create_from_proto query_id, exception
    create(
      stack: exception.stackEntry,
      count: exception.count,

      # TODO: Also add analyst here
      query_id: query_id
    )
  end
end
