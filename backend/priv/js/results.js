// utility function to check if a value is valid and
// return a fallback value in case it is not
function valid(value, fallback)
{
  return (typeof value !== 'undefined') ? value : fallback;
}

function process_result(JSONResult)
{
  var result = JSON.parse(JSONResult);

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

  aggregate_quantized_buckets(result);

  return JSON.stringify(result);
}
