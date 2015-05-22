require './lib/post_processors/ecdf'

class ResultHandler
  # Store the new sets of results
  def self.store_results task, json, published_at
    # create a new result
    bucket_json = post_process_buckets(json["buckets"]).to_json
    new_result = Result.new(task: task, buckets_json: bucket_json)
    new_result.analyst = task.analyst
    new_result.created_at = published_at
    new_result.save

    # copy all exceptions as exception_results
    if json["exceptions"] and json["exceptions"].size > 0
      json["exceptions"].each {|ex| ExceptionResult.create_from_json new_result, ex}
    end

    new_result
  end

  def self.post_processing_types buckets
    requested_post_processing_types = buckets.select do |bucket|
      bucket["label"] == "ac_postprocessing"
    end
    requested_post_processing_type_names = requested_post_processing_types.map do |bucket|
      bucket["value"]
    end
    requested_post_processing_type_names.select do |type|
      ["ecdf", "histogram"].include?(type)
    end
  end

  def self.post_process_buckets original_buckets
    post_processing_types(original_buckets).inject(original_buckets) do |buckets, type|
      ECDF.process buckets if type == "ecdf"
    end
  end
end
