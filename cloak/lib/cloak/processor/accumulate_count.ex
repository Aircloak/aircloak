defmodule Cloak.Processor.AccumulateCount do
  @moduledoc """
  Computes the accumulated total of an attribute across all users.
  This is very useful in the case where each user might have multiple
  of an entry that we would like to attempt to account for.

  The process is inaccurate at best due to the anonymisation, and
  long tails are completely lost.

  The process works by producing a CDF of the counts, in the pre-processing
  phase, and then constructing a single count in the post-processing phase.

  FIXME: Possible optimisations
  - have input ORDERED BY user to reduce the amount of data that is accumulated
  - anonymize directly inside processor, and then do post-processing immediately too
  """

  require Record
  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  @type accumulate_property :: {any, pos_integer}
  @type bucket :: record(:bucket, property: [accumulate_property], noisy_count: pos_integer)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Given a list of properties for each user, the pre-processor will do the following per user
  - count how many times each property occurrs
  - make a per property CDF that can be used to produce a global count per property

  It is assumed that the first column in each row is the ID of the user.
  """
  @spec pre_process([any]) :: [accumulate_property]
  def pre_process(database_rows) do
    database_rows
    |> group_by_user
    |> Enum.flat_map(&per_user_processing/1)
  end

  @doc """
  Produces aggregate total counts based on the anonymized output of the pre-processing step.

  The post-processor assumes that the buckets it receives were produced by the
  accumulate count pre-processor, but doesn't check that this is in fact the case.
  If it isn't it might fail, or otherwise produce incorrect answers.
  """
  @spec post_process([bucket]) :: [bucket]
  def post_process(anonymized_buckets) do
    {anonymized_values, low_count_buckets} = anonymized_buckets
    |> extract_from_buckets
    |> Enum.partition(fn({property, _}) -> property != ["aircloak_lcf_tail"] end)

    processed_values = anonymized_values
    |> group_by_property
    |> Enum.map(&aggregate_for_property/1)

    low_count_buckets ++ processed_values
    |> Enum.map(&format_as_buckets/1)
  end


  # -------------------------------------------------------------------
  # Internal functions for pre-processing
  # -------------------------------------------------------------------

  defp group_by_user(buckets) do
    buckets
    |> Enum.reduce(%{}, fn([user | property], accumulator) ->
        Map.update(accumulator, user, [property], fn(existing_properties) -> [property | existing_properties] end)
      end)
    |> Enum.to_list
  end

  defp per_user_processing({user, properties}) do
    properties
    |> Enum.group_by(&(&1))
    |> Enum.flat_map(fn({prop, occurences}) ->
      count = Enum.count(occurences)
      1..count |> Enum.map(&([user, {prop, &1}]))
    end)
  end


  # -------------------------------------------------------------------
  # Internal functions for post-processing
  # -------------------------------------------------------------------

  defp extract_from_buckets(anonymized_buckets) do
    Enum.map(anonymized_buckets, &({bucket(&1, :property), bucket(&1, :noisy_count)}))
  end

  defp group_by_property(anonymized_properties) do
    anonymized_properties
    |> Enum.reduce(%{}, fn({{prop, val}, count}, accumulator) ->
      Map.update(accumulator, prop, [{val, count}], &([{val, count} | &1]))
    end)
    |> Enum.to_list
    |> Enum.map(&descending_sorted_values/1)
  end

  defp descending_sorted_values({prop, values}) do
    sorted_values = values
    |> Enum.sort
    |> Enum.reverse
    {prop, sorted_values}
  end

  defp aggregate_for_property({property, values}) do
    {sum, _} = Enum.reduce(values, {0, 0}, fn({value, count}, {sum, prev_max_count}) ->
      new_sum = sum + value * max(count - prev_max_count, 0)
      next_prev_max_count = max(prev_max_count, count)
      {new_sum, next_prev_max_count}
    end)
    {property, sum}
  end

  defp format_as_buckets({property, count}) do
    bucket(property: property, noisy_count: count)
  end
end
