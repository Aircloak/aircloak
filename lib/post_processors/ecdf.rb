class ECDF
  def self.process buckets
    relevant_buckets = buckets.select {|bucket| bucket["label"] =~ /ac_ecdf_val/}
    original_meta_buckets = buckets.select do |bucket|
      bucket["label"] =~ /ac_postprocessing/ and bucket["value"] == "ecdf"
    end

    max_count = relevant_buckets.first["count"]
    # Find boundaries
    relevant_buckets.each do |bucket|
      val = bucket["count"]
      max_count = val if val > max_count
    end

    result_buckets = []
    relevant_buckets.each do |bucket|
      from_min = max_count - bucket["count"]
      percentage = (100 * from_min) / max_count
      result_buckets << {
        "label" => "ecdf_val",
        "value" => bucket["value"],
        "count" => percentage
      }
    end

    meta_buckets = [{
      "label" => "ac_query",
      "value" => "ecdf",
      "count" => max_count
    }]

    buckets - relevant_buckets + result_buckets - original_meta_buckets + meta_buckets
  end
end
