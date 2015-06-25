# create namespace for results-related shared variables
window.Results = window.Results or {}

# converts a number to string with at most 2 decimal places
format_number = (number) ->
  (Math.round(number * 100) / 100).toString()


# for a single quantized datum, compute the aggregated values
compute_aggregate_buckets = (dataBuckets, name, total) ->
  data = {}
  for bucket in dataBuckets
    if bucket.value[0] == '['
      params = bucket.value.substring(1, bucket.value.length - 1).split(',')
      min = parseInt(params[0])
      max = parseInt(params[1])
      step = parseInt(params[2])
      in_range = bucket.count
    else if bucket.value[0] == '<'
      value = parseInt(bucket.value.substring(1, bucket.value.length))
      data[value] = bucket.count

  countBucket = {label: "data", value: "in range", count: in_range}

  percentOfTotal = Math.round((in_range / total) * 20) * 5
  percentOfTotal = Math.min(Math.max(percentOfTotal, 0), 100)
  percentBucket = {label: "data", value: "% of total", count: "~ " + percentOfTotal + "%"}

  sum = 0
  count = -1
  for i in [min+step..max-step] by step
    diff = (data[i] ? 0) - (data[i - step] ? 0)
    count = count + diff
    sum = sum + diff * (i - step / 2)
  count = 1 if count < 1
  average = sum / count
  averageBucket = {label: "data", value: "average", count: format_number(average)}

  median = 0
  for i in [min+step..max-step] by step
    if (data[i] ? 0) >= total / 2
      median = i - step / 2
      break
  medianBucket = {label: "data", value: "median", count: format_number(median)}

  sum = 0
  for i in [min+step..max-step] by step
    diff = (data[i] ? 0) - (data[i - step] ? 0)
    variance = i - step / 2 - average
    sum = sum + diff * variance * variance
  stdDev = Math.sqrt(sum / count)
  stdDevBucket = {label: "data", value: "stdev.S", count: format_number(stdDev)}

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
