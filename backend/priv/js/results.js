// converts a number to string with at most 2 decimal places
function format_number(number)
{
  return (Math.round(number * 100) / 100).toString();
}

function number_to_key(number)
{
  return Math.round(number * 10000).toString(); // keep only 4 decimal places
}

function valid(value, fallback)
{
  return (typeof value !== 'undefined') ? value : fallback;
}

// this procedure significantly reduces the effect of the noise on the analysis of the CDFs, at the cost
// of slightly biasing the values towards the upper range; the bias could be canceled if we would also
// generate inverse CDFs, but the difference is small and it is not worth it
function process_cdfs(cdfs, min, max, step)
{
  // first, we average the values so we reduce the jitter from the noise
  var prev = 0;
  for (var i = min + step; i <= max; i += step)
  {
    var current = cdfs[number_to_key(i)];
    var next = valid(cdfs[number_to_key(i + step)], current);
    var average = (current + prev + next) / 3;
    cdfs[number_to_key(i)] = average
    prev = current
  }

  // now we make sure the values are monotonically increasing
  prev = 0
  for (var i = min + step; i <= max; i += step)
  {
    var current = cdfs[number_to_key(i)];
    if (current < prev) // check if we have a downward slope
    {
      var local_max = prev;

      // find slope ending (where values are bigger than the local maximum) and local minimum
      var local_min = current;
      for (var end = i + step; end <= max; end += step)
      {
        current = cdfs[number_to_key(end)];
        if (current < local_min)
          local_min = current;
        else if (current > local_max)
          break;
      }
      end = end - step;

      // find slope start (where values are smaller than local minimum)
      for (var start = i - 2 * step; start >= min; start -= step)
      {
        current = cdfs[number_to_key(start)];
        if (current < local_min)
          break;
      }
      start = start + step;

      // straighten slope
      for (var j = start; j <= end; j += step)
        cdfs[number_to_key(j)] = (local_min + local_max) / 2;

      // skip current interval
      i = end;
      current = local_max;
    }
    prev = current;
  }
}

// for a single quantized datum, compute the aggregated values
function aggregate_data_values(dataBuckets, name, total)
{
  var cdfs = {}, step = -1, min = 0, max = -1, in_range = -1;
  for (var i = 0; i < dataBuckets.length; i++)
  {
    var bucket = dataBuckets[i];
    if (bucket.value[0] === '[')
    {
      var params = bucket.value.substring(1, bucket.value.length - 1).split(',');
      min = parseFloat(params[0]);
      max = parseFloat(params[1]);
      step = parseFloat(params[2]);
      in_range = bucket.count;
    }
    else if (bucket.value[0] === '<')
    {
      var value = parseFloat(bucket.value.substring(1, bucket.value.length));
      cdfs[number_to_key(value)] = bucket.count;
    }
  }
  if (min >= max) throw "invalid data values (min >= max)";
  step = Math.min(max - min, step);

  // fill missing values
  cdfs[number_to_key(max)] = in_range;
  for (var i = min; i <= max; i += step)
    cdfs[number_to_key(i)] = valid(cdfs[number_to_key(i)], 0);

  var countBucket = {label: name, value: "values in range", count: in_range};

  var percentOfTotal = Math.round((in_range / total) * 20) * 5;
  percentOfTotal = Math.min(Math.max(percentOfTotal, 0), 100);
  var percentBucket = {label: name, value: "% of total values", count: "~ " + percentOfTotal + "%"};

  // we need smoother CDFs to reliably compute aggregate values
  process_cdfs(cdfs, min, max, step)

  // produce data histogram
  var histogram = {name: name, min: min, max: max, values: []};
  for(var i = min + step; i <= max; i += step)
  {
      var intervalCount = cdfs[number_to_key(i)] - cdfs[number_to_key(i - step)];
      histogram.values.push(intervalCount);
  }

  // reduce range
  for(var i = min + step; i <= max; i += step)
  {
    if (cdfs[number_to_key(i)] > 0)
    {
      min = i - step;
      break;
    }
  }
  var max_value = cdfs[number_to_key(max)];
  for(var i = max - step; i >= min; i -= step)
  {
    if (cdfs[number_to_key(i)] < max_value)
    {
      max = i + step;
      break;
    }
  }

  var sum = 0;
  var count = -1;
  for(var i = min + step; i <= max; i += step)
  {
    var diff = cdfs[number_to_key(i)] - cdfs[number_to_key(i - step)];
    count = count + diff;
    sum = sum + diff * (i - step / 2);
  }
  if (count < 1) count = 1;
  if (sum < 0) sum = 0;
  var average = sum / count;
  var averageBucket = {label: name, value: "average", count: format_number(average)};

  var median = 0;
  for(var i = min + step; i <= max; i += step)
  {
    var current = cdfs[number_to_key(i)];
    if (current >= in_range / 2)
    {
      // found middle section start, look for middle section end
      var j = i + step;
      while (j < max && cdfs[number_to_key(j)] <= current)
        j = j + step;
      j = j - step;
      median = (i - step + j) / 2;
      break;
    }
  }
  var medianBucket = {label: name, value: "median", count: format_number(median)};

  var sum = 0;
  for(var i = min + step; i <= max; i += step)
  {
    var diff = cdfs[number_to_key(i)] - cdfs[number_to_key(i - step)];
    var variance = i - step / 2 - average;
    var sum = sum + diff * variance * variance;
  }
  if (sum < 0) sum = 0;
  var stdDev = Math.sqrt(sum / count);
  var stdDevBucket = {label: name, value: "stdev.S", count: format_number(stdDev)};

  var minBucket = {label: name, value: "min", count: "< " + format_number(min)};
  var maxBucket = {label: name, value: "max", count: "> " + format_number(max)};

  var buckets = [countBucket, percentBucket, averageBucket, medianBucket, stdDevBucket, minBucket, maxBucket];

  return {buckets: buckets, histogram: histogram};
}

// aggregate buckets for a single quantized datum
function aggregate_quantized_bucket(result, quantizedBucket)
{
  var name = quantizedBucket.value;
  var total = quantizedBucket.count;
  var parts = _.partition(result.buckets, function(bucket) { return bucket.label === name; });
  var data = parts[0];
  var remainingBuckets = parts[1];
  var aggregatedData = aggregate_data_values(data, name, total);
  result.buckets = _.union(aggregatedData.buckets, remainingBuckets);
  result.histograms.push(aggregatedData.histogram);
}

// this function will remove the quantized buckets from the result and replace them with the computed aggregated buckets
function aggregate_quantized_buckets(result)
{
  // sort buckets
  bucketComparator = function(bucket1, bucket2)
  {
    var labelComparison = bucket1.label.localeCompare(bucket2.label);
    if (labelComparison !== 0)
      return labelComparison;
    var value1 = Number(bucket1.value);
    var value2 = Number(bucket2.value);
    if (isNaN(value1) || isNaN(value2))
      return bucket1.value.localeCompare(bucket2.value);
    else
      return value1 - value2;
  };
  result.buckets.sort(bucketComparator);

  // create histograms container
  result.histograms = [];

  // find quantized data
  parts = _.partition(result.buckets, function (bucket) { return bucket.label === "quantized"; });
  quantizedBuckets = parts[0];
  if(quantizedBuckets.length === 0)
      return; // nothing to compute
  result.buckets = parts[1];

  // compute aggregated buckets for each datum
  for (var i = 0; i < quantizedBuckets.length; i++)
    aggregate_quantized_bucket(result, quantizedBuckets[i]);
}

function process_result(JSONResult)
{
  var result = JSON.parse(JSONResult);
  aggregate_quantized_buckets(result);
  return JSON.stringify(result);
}
