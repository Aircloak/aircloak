class PropertyResult < ActiveRecord::Base
  belongs_to :property
  has_one :query, through: :property
  has_many :property_result_counts, dependent: :destroy

  attr_accessor :count

  def initialize
    count = 0
  end

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
