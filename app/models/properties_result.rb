class PropertiesResult < ActiveRecord::Base
  belongs_to :property
  has_one :query, through: :property

  def self.create_from_proto query_id, prop
    property = Property.from_proto query_id, prop
    create(
      numeric: prop.long_answer.blank?,
      long_value: prop.long_answer,
      str_value: prop.str_answer,
      count: prop.count,
      property: property
    )
  end
end
