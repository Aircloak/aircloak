require './lib/proto/air/aggregate_results.pb'

class Result < ActiveRecord::Base
  belongs_to :task
  has_many :buckets, dependent: :destroy

  def to_result_proto
    bs = buckets.map(&:to_bucket_proto)
    ResultPB.new(analyst_id: task.analyst, task_id: task.id, index: task.index, result_id: result_id,
        buckets: bs, exceptions: [])
  end
end
