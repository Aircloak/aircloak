require 'json'

class ECDF
  def self.process buckets
    # Validate that it is in fact an ECDF we are seeing here.
    # If not, we short circuit the subsequent processing steps.
    return buckets unless buckets.any? do |bucket|
      bucket["label"] =~ /ac_postprocessing/ and bucket["value"] == "ecdf"
    end

    # We will prepare our output here
    result = {
      "label" => "ac_graph",
      "count" => 1, # This is a dummy value, which is expected, but won't be used
      "value" => "" # Will be replaced with the actual data
    }

    ecdf_data = {
      type: "ecdf"
    }

    # The identifiers we look for, for things like labels, legend and title
    meta_identifiers = [
      "x_label",
      "y_label",
      "legend",
      "title"
    ]
    meta_buckets = buckets.select do |bucket|
      label = bucket["label"]
      was_meta_bucket = false
      was_meta_bucket = true if label == "ac_postprocessing"

      meta_identifiers.each do |identifier|
        if label == "ac_ecdf_#{identifier}"
          ecdf_data[identifier] = bucket["value"]
          was_meta_bucket = true
        end
      end
      # Whether we should keep it as a bucket or not.
      # The buckets we keep, we filter out of the bucket list.
      was_meta_bucket
    end

    # These buckets contain the numerical values used to render the ECDF
    data_buckets = buckets.select {|bucket| bucket["label"] =~ /ac_ecdf_val/}

    max_count = data_buckets.first["count"]
    # Find boundaries
    data_buckets.each do |bucket|
      val = bucket["count"]
      max_count = val if val > max_count
    end

    result_buckets = []
    data_buckets.each do |bucket|
      from_min = max_count - bucket["count"]
      percentage = (100 * from_min) / max_count
      result_buckets << {x: bucket["value"].to_i, y: percentage}
    end

    result_buckets.sort_by! {|bucket| bucket[:x]}
    begin
      did_adjustment = false
      result_buckets.each_index do |index|
        next if index == 0
        prev = result_buckets[index-1][:y]
        current = result_buckets[index][:y]
        if prev > current
          # We are seeing effects of noise.
          # Let's fake it by averaging them out
          average = (prev + current) / 2
          result_buckets[index-1][:y] = average
          result_buckets[index][:y] = average
          did_adjustment = true
        end
      end
    end while(did_adjustment)

    ecdf_data["data"] = result_buckets

    result["value"] = ecdf_data.to_json
    # There might be other post-processors in line here as well
    # so we only filter out the buckets we have consumed.
    buckets - data_buckets - meta_buckets + [result]
  end
end
