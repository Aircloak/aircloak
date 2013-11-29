require './lib/proto/air/aggregate_results.pb'

class Result < ActiveRecord::Base
  belongs_to :query
  has_many :buckets, dependent: :destroy

  def to_result_proto
    props = buckets.map(&:to_property_proto)
    ResultProto.new(analyst_id: query.analyst, task_id: query.id, index: query.index, result_id: result_id,
        properties: props, exceptions: [])
  end
end
