// utility function to check if a value is valid and
// return a fallback value in case it is not
function valid(value, fallback)
{
  return (typeof value !== 'undefined') ? value : fallback;
}

// Notice: result buckets have to be sorted by label for this to work properly
function extract_buckets(result, name)
{
  var buckets = result.buckets;
  var start = 0;
  while(start < buckets.length && buckets[start].label !== name)
    start++;
  if (start === buckets.length)
    return [];
  var end = start + 1;
  while (end < buckets.length && buckets[end].label === name)
    end++;
  return buckets.splice(start, end - start);
}

function extract_aircloak_buckets(result)
{
  var aircloak_buckets = extract_buckets(result, "aircloak");

  for (var i = 0; i < aircloak_buckets.length; i++)
  {
    var bucket = aircloak_buckets[i];
    result.post_processed.aircloak[bucket.value] = bucket.count;
  }
}

function insert_buckets(result, newBuckets)
{
  result.buckets.unshift.apply(result.buckets, newBuckets);
}

function process_result(JSONResult)
{
  var result = JSON.parse(JSONResult);
  do_process_result(result);
  return JSON.stringify(result);
}

function do_process_result(result) {
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

  // create post-processed data container
  result.post_processed = {};
  result.post_processed.aircloak = {};

  extract_aircloak_buckets(result)
  aggregate_accumulator_buckets(result);
  aggregate_quantized_buckets(result);
}

function process_stats(JSONResult)
{
  var result = JSON.parse(JSONResult), num_rows = 0, num_users = 0;

  // There were errors, so report failure
  if (result.exceptions.length > 0)
    return JSON.stringify({success: false, exceptions: result.exceptions});

  // Run standard task postprocessing to handle special columns
  do_process_result(result);

  // Extract data and return
  num_users_buckets = extract_buckets(result, "num_users");
  if (num_users_buckets.length == 1)
    num_users = num_users_buckets[0].count;

  num_rows_buckets = extract_buckets(result, "num_rows");
  if (num_rows_buckets.length == 1)
    num_rows = num_rows_buckets[0].count;

  return JSON.stringify({success: true, num_rows: num_rows, num_users: num_users});
}
