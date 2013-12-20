require './lib/proto/air/aggregate_results.pb'

class ResultHandler
  # Store the new sets of results
  def self.store_results task, proto
    # create a new result
    new_result = Result.new(query: task, result_id: proto.result_id)
    # copy all properties as buckets
    if proto.properties
      proto.properties.each do |property|
        bucket = create_bucket_for_property property
        new_result.buckets << bucket
      end
    end
    new_result.save
  end

  # create a new bucket for a given property
  def self.create_bucket_for_property property
    label = property.label
    str_answer = property.string
    range_min = property.range.min if property.range
    range_max = property.range.max if property.range
    if property.joiners_leavers
      joiners = property.joiners_leavers.joiners
      leavers = property.joiners_leavers.leavers
    else
      joiners = nil
      leavers = nil
    end
    Bucket.create(label: label, str_answer: str_answer, range_min: range_min, range_max: range_max,
        joiners: joiners, leavers: leavers, accumulated_count: property.accumulated_count)
  end
end
