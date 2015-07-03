# create namespace for results-related shared variables
window.Results = window.Results or {}

# converts a number to string with at most 2 decimal places
format_number = (number) ->
  (Math.round(number * 100) / 100).toString()

number_to_key = (number) ->
  # keep only 4 decimal places
  Math.round(number * 10000)

# for a single quantized datum, compute the aggregated values
compute_aggregate_buckets = (dataBuckets, name, total) ->
  data = {}
  for bucket in dataBuckets
    if bucket.value[0] == '['
      params = bucket.value.substring(1, bucket.value.length - 1).split(',')
      min = parseFloat(params[0])
      max = parseFloat(params[1])
      step = parseFloat(params[2])
      in_range = bucket.count
    else if bucket.value[0] == '<'
      value = parseFloat(bucket.value.substring(1, bucket.value.length))
      data[number_to_key(value)] = bucket.count
  return [] if min >= max
  step = Math.min(max - min, step)
  data[number_to_key(max)] = in_range

  countBucket = {label: name, value: "values in range", count: in_range}

  percentOfTotal = Math.round((in_range / total) * 20) * 5
  percentOfTotal = Math.min(Math.max(percentOfTotal, 0), 100)
  percentBucket = {label: name, value: "% of total values", count: "~ " + percentOfTotal + "%"}

  sum = 0
  count = -1
  for i in [min+step..max] by step
    diff = (data[number_to_key(i)] ? 0) - (data[number_to_key(i - step)] ? 0)
    count = count + diff
    sum = sum + diff * (i - step / 2)
  count = 1 if count < 1
  sum = 0 if sum < 0
  average = sum / count
  averageBucket = {label: name, value: "average", count: format_number(average)}

  median = 0
  for i in [min+step..max] by step
    if (data[number_to_key(i)] ? 0) >= in_range / 2
      median = i - step / 2
      break
  medianBucket = {label: name, value: "median", count: format_number(median)}

  sum = 0
  for i in [min+step..max] by step
    diff = (data[number_to_key(i)] ? 0) - (data[number_to_key(i - step)] ? 0)
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
