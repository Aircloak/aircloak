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
    min_count = max_count
    # Find boundaries
    data_buckets.each do |bucket|
      val = bucket["count"]
      max_count = val if val > max_count
      min_count = val if val < min_count
    end

    result_buckets = []
    data_buckets.each do |bucket|
      from_min = max_count - bucket["count"] + min_count
      percentage = (100 * from_min) / max_count
      result_buckets << {x: bucket["value"].to_i, y: percentage}
    end

    result_buckets.sort_by! {|bucket| bucket[:x]}
    # Due to the noise in the buckets reported from the cloak,
    # we end up with ECDF artefacts where the supposedly monotonically
    # increasing curve all of a sudden dips down.
    # We counteract this in two steps:
    # 1. we average out the values around the dips
    #    to account for the noise
    # 2. we force each subsequent
    #    value to be as great or greater than
    #    the value preceding it.
    #
    # While one could manage with step 1 alone
    # by repeatedly performing it until the
    # curve is monotonic, it is hard to predict
    # how long it will actually take to complete.
    # To limit runtime we therefore approximate it
    # with step 2 instead.

    # Phase 1: average
    # We average based on the original values.
    # This means that the sequence
    #   4, 3, 2, 1
    # becomes:
    #   3.5, 2.5, 1.5, 1.5
    # and:
    #   1, 0, 1, 2
    # becomes:
    #   0.5, 0.5, 0.5, 2
    orig_prev = nil
    result_buckets.each_index do |index|
      if index == 0
        orig_prev = result_buckets[index][:y]
      else
        current = result_buckets[index][:y]
        if orig_prev > current
          # We are seeing effects of noise.
          # Let's fake it by averaging them out
          average = (orig_prev + current) / 2
          result_buckets[index-1][:y] = average
          result_buckets[index][:y] = average
        end
        orig_prev = current
      end
    end
    # Phase 2: force monotonicity
    result_buckets.each_index do |index|
      next if index == 0
      prev = result_buckets[index-1][:y]
      current = result_buckets[index][:y]
      result_buckets[index][:y] = prev if prev > current
    end

    # The averaging and shifting of the values might have changed the scale
    # of the values, such that the maximum value no longer is at 100%
    # We need to re-scale in order to ensure we end up with a total of 100%
    current_max_val = result_buckets.last[:y]
    ratio_to_hundred = 100.0 / current_max_val
    result_buckets.map! do |bucket|
      bucket[:y] *= ratio_to_hundred
      bucket
    end

    ecdf_data["data"] = result_buckets

    result["value"] = ecdf_data.to_json
    # There might be other post-processors in line here as well
    # so we only filter out the buckets we have consumed.
    buckets - data_buckets - meta_buckets + [result]
  end
end
