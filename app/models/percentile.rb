class Percentile < ActiveRecord::Base
  has_many :percentile_results, dependent: :destroy
  belongs_to :query

  def self.from_proto query_id, prop
    p = Percentile.first(bucket: prop.name, query_id: query_id)
    p = Percentile.create(bucket: prop.name, query_id: query_id) unless p
    p
  end
end
