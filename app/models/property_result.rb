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

    numeric = prop.long_answer.blank?
    long_value = prop.long_answer
    str_value = prop.str_answer
    count = prop.count

    # Check to see if we have a property result 
    # for this particular property
    pr = if numeric
      PropertyResult.where(property_id: property.id, long_value: long_value).first
    else
      PropertyResult.where(property_id: property.id, str_value: str_value).first
    end
    pr = create(
      numeric: numeric,
      long_value: long_value,
      str_value: str_value,
      property_id: property.id
    ) unless pr
    pr.property_result_counts.create(count: count)
  end
end
