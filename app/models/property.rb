class Property < ActiveRecord::Base
  belongs_to :query
  has_many :property_results, dependent: :destroy

  def self.from_proto query_id, prop
    p = Property.first(property: prop.name, query_id: query_id)
    p = Property.create(property: prop.name, query_id: query_id) unless p
    p
  end
end
