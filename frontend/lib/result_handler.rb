class ResultHandler
  # Store the new sets of results
  def self.store_results task, json, published_at
    # create a new result
    new_result = Result.create(task: task, buckets_json: json["buckets"].to_json)
    new_result.analyst = task.analyst
    new_result.created_at = published_at
    new_result.save

    # copy all exceptions as exception_results
    if json["exceptions"] and json["exceptions"].size > 0
      json["exceptions"].each {|ex| ExceptionResult.create_from_json new_result, ex}
    end

    new_result
  end
end
