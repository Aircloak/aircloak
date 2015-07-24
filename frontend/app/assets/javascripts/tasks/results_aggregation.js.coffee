# create namespace for results-related shared variables
window.Results or= {}


# converts a number to string with at most 2 decimal places
format_number = (number) ->
  (Math.round(number * 100) / 100).toString()


number_to_key = (number) ->
  # keep only 4 decimal places
  Math.round(number * 10000).toString()


# this procedure significantly reduces the effect of the noise on the analysis of the CDFs, at the cost
# of slightly biasing the values towards the upper range; the bias could be canceled if we would also
# generate inverse CDFs, but the difference is small and it is not worth it
process_cdfs = (cdfs, min, max, step) ->
  # first, we average the values so we reduce the jitter from the noise
  prev = 0
  for i in [min+step..max] by step
    current = cdfs[number_to_key(i)]
    next = cdfs[number_to_key(i + step)] ? current
    average = (current + prev + next) / 3
    cdfs[number_to_key(i)] = average
    prev = current

  # now we make sure the values are monotonically increasing
  i = min + step
  prev = 0
  while i <= max
    current = cdfs[number_to_key(i)]
    if current < prev # check if we have a downward slope
      local_max = prev

      # find slope ending (where values are bigger than the local maximum) and local minimum
      local_min = current
      for end in [i+step..max] by step
        current = cdfs[number_to_key(end)]
        if current < local_min
          local_min = current
        else if current > local_max
          break
      end = end - step

      # find slope start (where values are smaller than local minimum)
      for start in [i-2*step..min] by -step
        current = cdfs[number_to_key(start)]
        if current < local_min
          break
      start = start + step

      # straighten slope
      for j in [start..end] by step
        cdfs[number_to_key(j)] = (local_min + local_max) / 2

      # skip current interval
      i = end + step
      prev = local_max
    else
      i = i + step
      prev = current

# for a single quantized datum, compute the aggregated values
compute_aggregate_buckets = (dataBuckets, name, total, plot_data_callback) ->
  cdfs = {}
  for bucket in dataBuckets
    if bucket.value[0] == '['
      params = bucket.value.substring(1, bucket.value.length - 1).split(',')
      min = parseFloat(params[0])
      max = parseFloat(params[1])
      step = parseFloat(params[2])
      in_range = bucket.count
    else if bucket.value[0] == '<'
      value = parseFloat(bucket.value.substring(1, bucket.value.length))
      cdfs[number_to_key(value)] = bucket.count
  return [] if min >= max
  step = Math.min(max - min, step)

  # fill missing values
  cdfs[number_to_key(max)] = in_range
  for i in [min..max] by step
    if !cdfs[number_to_key(i)]
      cdfs[number_to_key(i)] = 0

  countBucket = {label: name, value: "values in range", count: in_range}

  percentOfTotal = Math.round((in_range / total) * 20) * 5
  percentOfTotal = Math.min(Math.max(percentOfTotal, 0), 100)
  percentBucket = {label: name, value: "% of total values", count: "~ " + percentOfTotal + "%"}

  # we need smoother CDFs to reliably compute aggregate values
  process_cdfs cdfs, min, max, step

  # invoke plot data callback, if any supplied
  if plot_data_callback
    data = []
    steps = (max - min) / step
    # set bigger plot step to avoid congestion of data on graph and to reduce jitter
    plot_step = step *
        if steps >= 500 then 20
        else if steps >= 200 then 10
        else if steps >= 100 then 5
        else if steps >= 50 then 2
        else 1
    for i in [min+plot_step..max] by plot_step
      diff = cdfs[number_to_key(i)] - cdfs[number_to_key(i - plot_step)]
      data.push {x: i - plot_step / 2, y: diff}
    plot_data_callback name, data, plot_step

  # reduce range
  for i in [min+step..max] by step
    if cdfs[number_to_key(i)] > 0
      min = i - step
      break
  max_value = cdfs[number_to_key(max)]
  for i in [max-step..min] by -step
    if cdfs[number_to_key(i)] < max_value
      max = i + step
      break

  sum = 0
  count = -1
  for i in [min+step..max] by step
    diff = cdfs[number_to_key(i)] - cdfs[number_to_key(i - step)]
    count = count + diff
    sum = sum + diff * (i - step / 2)
  count = 1 if count < 1
  sum = 0 if sum < 0
  average = sum / count
  averageBucket = {label: name, value: "average", count: format_number(average)}

  median = 0
  for i in [min+step..max] by step
    current = cdfs[number_to_key(i)]
    if current >= in_range / 2
      # found middle section start, look for middle section end
      j = i + step
      while j < max and cdfs[number_to_key(j)] <= current
        j = j + step
      j = j - step
      median = (i - step + j) / 2
      break
  medianBucket = {label: name, value: "median", count: format_number(median)}

  sum = 0
  for i in [min+step..max] by step
    diff = cdfs[number_to_key(i)] - cdfs[number_to_key(i - step)]
    variance = i - step / 2 - average
    sum = sum + diff * variance * variance
  sum = 0 if sum < 0
  stdDev = Math.sqrt(sum / count)
  stdDevBucket = {label: name, value: "stdev.S", count: format_number(stdDev)}

  minBucket = {label: name, value: "min", count: "< " + format_number(min)}
  maxBucket = {label: name, value: "max", count: "> " + format_number(max)}

  [countBucket, percentBucket, averageBucket, medianBucket, stdDevBucket, minBucket, maxBucket]


# aggregate buckets for a single quantized datum
aggregate_quantized_bucket = (buckets, quantized_bucket, plot_data_callback) ->
  name = quantized_bucket.value
  total = quantized_bucket.count
  parts = _.partition buckets, (bucket) ->
        bucket.label == name
  data = parts[0]
  buckets = parts[1]
  aggregate_buckets = compute_aggregate_buckets data, name, total, plot_data_callback
  _.union aggregate_buckets, buckets


# this function will remove the quantized buckets from the result and
# replace them with the computed aggregated buckets
Results.aggregate_quantized_buckets = (buckets, plot_data_callback) ->
  # sort buckets
  bucketComparator = (bucket1, bucket2) ->
      labelComparison = bucket1.label.localeCompare(bucket2.label)
      return labelComparison if labelComparison != 0
      value1 = Number(bucket1.value)
      value2 = Number(bucket2.value)
      if isNaN(value1) or isNaN(value2)
        return bucket1.value.localeCompare(bucket2.value)
      else
        return value1 - value2
  buckets.sort bucketComparator
  # find quantized data
  parts = _.partition buckets, (bucket) ->
        bucket.label == "quantized"
  quantized_buckets = parts[0]
  buckets = parts[1]
  # compute aggregated buckets for each datum
  for quantized_bucket in quantized_buckets
    buckets = aggregate_quantized_bucket buckets, quantized_bucket, plot_data_callback
  buckets
