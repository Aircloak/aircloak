class ResultHandler
  # Store the new sets of results
  def self.store_results task, json, published_at
    # create a new result
    new_result = Result.create(task: task)
    new_result.analyst = task.analyst
    new_result.created_at = published_at
    # copy all properties as buckets
    create_buckets json["buckets"], new_result.id if json["buckets"] and json["buckets"].size > 0
    # copy all exceptions as exception_results
    if json["exceptions"] and json["exceptions"].size > 0
      json["exceptions"].each {|ex| ExceptionResult.create_from_json new_result, ex}
    end
  end

  # inserts all the buckets in a single SQL statement
  def self.create_buckets buckets, result_id
    inserts = []
    buckets.each do |bucket|
      label = Bucket.sanitize bucket["label"]
      value = Bucket.sanitize bucket["value"]
      count = Bucket.sanitize bucket["count"]
      inserts.push "(#{result_id}, #{label}, #{value}, #{count})"
    end
    sql = "INSERT INTO buckets (result_id, label, value, count) VALUES #{inserts.join(", ")}"
    Bucket.connection.execute sql
  end
end
