class PropertiesResult < ActiveRecord::Base
  belongs_to :query

  def self.create_from_proto query_id, prop
    create(
      bucket: prop.name,
      numeric: prop.long_answer.blank?,
      long_value: prop.long_answer,
      str_value: prop.str_answer,
      count: prop.count,

      # TODO: Also add analyst here
      query_id: query_id
    )
  end
end
