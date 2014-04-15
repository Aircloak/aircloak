require './lib/proto/air/aggregate_results.pb'

class ResultHandler
  # Store the new sets of results
  def self.store_results task, proto
    # create a new result
    new_result = Result.create(query: task, result_id: proto.result_id)
    # copy all properties as buckets
    create_buckets_for_properties proto.properties, new_result.id if proto.properties
  end

  # inserts all the buckets in a single SQL statement
  def self.create_buckets_for_properties properties, result_id
    inserts = []
    properties.each do |property|
      label = Bucket.sanitize property.label
      str_answer = Bucket.sanitize property.string
      range_min = property.range ? Bucket.sanitize(property.range.min) : 0
      range_max = property.range ? Bucket.sanitize(property.range.max) : 0
      if property.joiners_leavers
        joiners = Bucket.sanitize property.joiners_leavers.joiners
        leavers = Bucket.sanitize property.joiners_leavers.leavers
      else
        joiners = 0
        leavers = 0
      end
      accumulated_count = Bucket.sanitize property.accumulated_count
      inserts.push "(#{result_id}, #{label}, #{str_answer}," +
          " #{range_min}, #{range_max}, #{joiners}, #{leavers}, #{accumulated_count})"
    end
    sql = "INSERT INTO buckets (result_id, label, str_answer, range_min, range_max, " +
        "joiners, leavers, accumulated_count) VALUES #{inserts.join(", ")}"
    Bucket.connection.execute sql
  end
end
