// This modules handles post-processing for accumulated properties.
// Accumulated properties are estimations of the sum of the property across all users having that property.
// The property has to be a positive integer value.
// We have two different types of accumulators: fast and cdf.
// "Fast" accumulators have lower accuracy, but generate less buckets and can handle higher ranges.
// "Cdf" accumulators give a better estimate, with the drawback of potentially generating large amounts of buckets.

// aggregate data buckets into a single sum bucket using the "fast" method
function fast_accumulate_data_values(dataBuckets, name)
{
  var sum = 0;
  for (var i = 0; i < dataBuckets.length; i++)
  {
    var bucket = dataBuckets[i];
    var term = parseInt(bucket.value);
    sum += term * bucket.count;
  }

  var totalBucket = {label: name, value: "total", count: sum};
  return [totalBucket];
}

// compute the "fast" sum bucket for each accumulated property
function aggregate_fast_accumulator_bucket(result, accumulatedBucket)
{
  var name = accumulatedBucket.value;
  var data = extract_buckets(result, name);
  var accumulatedBuckets = fast_accumulate_data_values(data, name);
  insert_buckets(result, accumulatedBuckets);
}

// aggregate data buckets into a single sum bucket using the "cdf" method
function cdf_accumulate_data_values(dataBuckets, name)
{
  var cdfs = {}, max = 0, min = 0;
  for (var i = 0; i < dataBuckets.length; i++)
  {
    var bucket = dataBuckets[i];
    var value = parseInt(bucket.value);
    cdfs[value] = bucket.count;
    max = Math.max(max, value);
    min = Math.min(min, value);
  }

  var sum = 0, prevMaxCount = 0;
  //sum positive terms
  while (max > 0)
  {
    var count = valid(cdfs[max], 0);
    sum += max * Math.max(count - prevMaxCount, 0)
    prevMaxCount = Math.max(prevMaxCount, count);
    max--;
  }
  //sum negative terms
  prevMaxCount = 0
  while (min < 0)
  {
    var count = valid(cdfs[min], 0);
    sum += min * Math.max(count - prevMaxCount, 0)
    prevMaxCount = Math.max(prevMaxCount, count);
    min++;
  }

  var totalBucket = {label: name, value: "total", count: sum};
  return [totalBucket];
}

// compute the "cdf" sum bucket for each accumulated property
function aggregate_cdf_accumulator_bucket(result, accumulatedBucket)
{
  var name = accumulatedBucket.value;
  var data = extract_buckets(result, name);
  var accumulatedBuckets = cdf_accumulate_data_values(data, name);
  insert_buckets(result, accumulatedBuckets);
}

// this function will remove the accumulated buckets from the result and replace them with the computed sum buckets
function aggregate_accumulator_buckets(result)
{
  // find fast accumulated data
  var accumulatedBuckets = extract_buckets(result, "fast_accumulator");
  if(accumulatedBuckets.length !== 0)
  {
    // compute accumulator bucket for each datum
    for (var i = 0; i < accumulatedBuckets.length; i++)
      aggregate_fast_accumulator_bucket(result, accumulatedBuckets[i]);
  }
  // find CDF accumulated data
  var accumulatedBuckets = extract_buckets(result, "cdf_accumulator");
  if(accumulatedBuckets.length !== 0)
  {
    // compute accumulator bucket for each datum
    for (var i = 0; i < accumulatedBuckets.length; i++)
      aggregate_cdf_accumulator_bucket(result, accumulatedBuckets[i]);
  }
}
