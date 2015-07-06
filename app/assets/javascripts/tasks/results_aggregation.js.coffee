# create namespace for results-related shared variables
window.Results = window.Results or {}


# converts a number to string with at most 2 decimal places
format_number = (number) ->
  (Math.round(number * 100) / 100).toString()


number_to_key = (number) ->
  # keep only 4 decimal places
  Math.round(number * 10000)


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
      end = i + step
      local_min = current
      while end <= max
        current = cdfs[number_to_key(end)]
        if current < local_min
          local_min = current
        else if current > local_max
          break
        end = end + step
      end = end - step

      # find slope start (where values are smaller than local minimum)
      start = i - 2 * step
      while start >= min
        current = cdfs[number_to_key(start)]
        if current < local_min
          break
        start = start - step
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
compute_aggregate_buckets = (dataBuckets, name, total) ->
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
    else
      break

  countBucket = {label: name, value: "values in range", count: in_range}

  percentOfTotal = Math.round((in_range / total) * 20) * 5
  percentOfTotal = Math.min(Math.max(percentOfTotal, 0), 100)
  percentBucket = {label: name, value: "% of total values", count: "~ " + percentOfTotal + "%"}

  # we need smoother CDFs to reliably compute aggregate values
  process_cdfs cdfs, min, max, step

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

  [countBucket, percentBucket, averageBucket, medianBucket, stdDevBucket]


# aggregate buckets for a single quantized datum
aggregate_quantized_bucket = (buckets, quantized_bucket) ->
  name = quantized_bucket.value
  total = quantized_bucket.count
  parts = _.partition buckets, (bucket) ->
        bucket.label == name
  data = parts[0]
  buckets = parts[1]
  aggregate_buckets = compute_aggregate_buckets data, name, total
  _.union buckets, aggregate_buckets


# this function will remove the quantized buckets from the result and
# replace them with the computed aggregated buckets
Results.aggregate_quantized_buckets = (buckets) ->
  # find quantized data
  parts = _.partition buckets, (bucket) ->
        bucket.label == "quantized"
  quantized_buckets = parts[0]
  buckets = parts[1]
  # compute aggregated buckets for each datum
  for quantized_bucket in quantized_buckets
    buckets = aggregate_quantized_bucket buckets, quantized_bucket
  buckets
