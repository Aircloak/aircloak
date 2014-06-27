require './lib/proto/air/aggregate_results.pb'

class Result < ActiveRecord::Base
  belongs_to :task
  belongs_to :analyst
  has_many :buckets, dependent: :destroy

  # This does an efficient SQL delete, rather than
  # loading all the data, running all the validations
  # and callbacks, etc
  def efficient_delete
    Bucket.connection.execute "DELETE FROM buckets WHERE result_id = #{self.id}"
    destroy
  end

  def to_result_proto
    bs = buckets.map(&:to_bucket_proto)
    ResultPB.new(analyst_id: task.analyst, task_id: task.id, index: task.index, buckets: bs, exceptions: [])
  end
end
