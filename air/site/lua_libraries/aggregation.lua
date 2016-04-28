Aircloak.Aggregation = {}

-- This function is useful if you need to calculate the sum of an integer value over all
-- users.
--
-- The properties reported by the function will be postprocessed before pushed to clients.
-- Hence, if you invoke Aircloak.Aggregation.fast_accumulate_property('some_property', some_value)
-- the final postprocessed result will contain the bucket {'some_property', sum_of_all_values}
-- where sum_of_all_values is the anonymized sum of all values reported under the given name.
--
-- As a result of the algorithm used, the reported values will be "compressed", i.e. they will
-- fall in the range of [1, floor(sqrt(2*value)) + 1], so you can expect larger counts per
-- aggregated buckets. Consequently, there's less chance of filtering out some buckets, and
-- even when this happens, such buckets will contribute less to the total sum. Therefore,
-- the anonymizer noise should be reduced.
--
function Aircloak.Aggregation.fast_accumulate_property(name, value)
  -- validate supplied parameters
  assert(type(value) == "number" and value == math.floor(value),
    "invalid value supplied to fast accumulate property call (must be integer)")
  local sign = 1
  if value < 0 then
    sign = -1
    value = -value
  end
  report_property("fast_accumulator", name)

  -- The function works by reporting the input value as a series of smaller integers whose sum
  -- is equal to the input value.
  --
  -- The function relies on the fact that sum of consecutive integers:
  -- 1 + 2 + ... + n = (n^2 + n)/2.
  -- Hence, if we choose n = sqrt(2*value), the sum of all integers will be
  -- value + sqrt(2*value)/2 which is slightly above the input value
  --
  -- To ensure that sum of produced integers is exactly equal to the input value, we start with
  -- floor(sqrt(2*value)) + 1. If we summed all integers from 1 up to that value, we'd get a
  -- slight overflow. Therefore, we report this integer, then subtract it from the input value,
  -- and repeat the process.
  -- As a result, we'll skip some integers in the sequence 1, 2, ..., floor(sqrt(2*value)) + 1
  -- and the final sum will be equal to the input value.
  --
  -- If the value is <= 3 we simply report it, because the algorithm above doesn't produce
  -- correct results for such small values
  while value > 3 do
    local term = math.floor(math.sqrt(2 * value)) + 1
    report_property(name, sign * term)
    value = value - term
  end
  if value > 0 then
    report_property(name, sign * value)
  end
end

-- sum estimation by reporting the CDF values for each property
function Aircloak.Aggregation.accumulate_property(name, value)
  -- validate supplied parameters
  assert(type(value) == "number" and value == math.floor(value),
    "invalid value supplied to accumulate property call (must be integer)")
  local sign = 1
  if value < 0 then
    sign = -1
    value = -value
  end
  report_property("cdf_accumulator", name)
  while value > 0 do
    report_property(name, sign * value)
    value = value - 1
  end
end
