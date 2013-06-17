class Property < ActiveRecord::Base
  belongs_to :query
  has_many :properties_results, dependent: :destroy

  def self.from_proto query_id, prop
    p = Property.where(property: prop.name, query_id: query_id).first
    p = Property.create(property: prop.name, query_id: query_id) unless p
    p
  end
end
