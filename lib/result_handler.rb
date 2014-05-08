require './lib/proto/air/aggregate_results.pb'

class ResultHandler
  # Store the new sets of results
  def self.store_results task, proto
    # create a new result
    new_result = Result.create(query: task, result_id: proto.result_id)
    # copy all properties as buckets
    create_buckets proto.buckets, new_result.id if proto.buckets
  end

  # inserts all the buckets in a single SQL statement
  def self.create_buckets buckets, result_id
    inserts = []
    buckets.each do |bucket|
      label = Bucket.sanitize bucket.label
      str_answer = Bucket.sanitize bucket.string
      range_min = bucket.range ? Bucket.sanitize(bucket.range.min) : 0
      range_max = bucket.range ? Bucket.sanitize(bucket.range.max) : 0
      accumulated_count = Bucket.sanitize bucket.accumulated_count
      inserts.push "(#{result_id}, #{label}, #{str_answer}," +
          " #{range_min}, #{range_max}, #{accumulated_count})"
    end
    sql = "INSERT INTO buckets (result_id, label, str_answer, range_min, range_max, " +
        "accumulated_count) VALUES #{inserts.join(", ")}"
    Bucket.connection.execute sql
  end
end
