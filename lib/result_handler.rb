require './lib/proto/air/aggregate_results.pb'

class ResultHandler
  # Store the new sets of results
  def self.store_results task, proto, published_at
    # create a new result
    new_result = Result.create(task: task)
    new_result.analyst = task.analyst
    new_result.created_at = published_at
    # copy all properties as buckets
    create_buckets proto.buckets, new_result.id unless proto.buckets.blank?
    # copy all exceptions as exception_results
    unless proto.exceptions.blank?
      proto.exceptions.each {|ex| ExceptionResult.create_from_proto new_result, ex}
    end
  end

  # inserts all the buckets in a single SQL statement
  def self.create_buckets buckets, result_id
    inserts = []
    buckets.each do |bucket|
      label = Bucket.sanitize bucket.label
      str_answer = Bucket.sanitize bucket.string
      accumulated_count = Bucket.sanitize bucket.accumulated_count
      inserts.push "(#{result_id}, #{label}, #{str_answer}, #{accumulated_count})"
    end
    sql = "INSERT INTO buckets (result_id, label, str_answer, accumulated_count) VALUES #{inserts.join(", ")}"
    Bucket.connection.execute sql
  end
end
