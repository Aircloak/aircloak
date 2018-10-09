defmodule DataQuality.Test.Query do
  @moduledoc "Executes queries and collects query results for further processing"

  alias Aircloak.OutputStatus
  alias DataQuality.Distributions
  alias DataQuality.Test
  alias DataQuality.Test.{Utility, Logger}

  @approximation_of_zero 0.000000000001

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec measure([Test.test()], Test.config(), [Test.dimension()]) :: [Test.result()]
  @doc "Queries the configured data sources producing raw results for further processing and analysis"
  def measure(tests, config, dimensions) do
    Logger.banner("Querying datasources")
    Enum.flat_map(tests, &collect_measurements(config, dimensions, &1))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp collect_measurements(config, dimensions, test) do
    Logger.header(test[:name])
    Enum.flat_map(Distributions.list(), &measurements_for_distribution(&1, config, dimensions, test))
  end

  defp measurements_for_distribution(distribution, config, dimensions, test) do
    distribution_name = Distributions.distribution_name(distribution)
    OutputStatus.new_line(distribution_name, :pending, "querying")
    results = Enum.flat_map(dimensions, &measurements_for_dimension(&1, distribution_name, config, test))
    OutputStatus.done(distribution_name)
    results
  end

  def measurements_for_dimension(dimension, distribution_name, config, test) do
    case run_queries(config, distribution_name, dimension, test[:aggregates]) do
      {:ok, dimension_results} ->
        Enum.map(
          dimension_results,
          &Map.merge(&1, %{
            dimension: dimension,
            distribution: distribution_name,
            class: test[:name]
          })
        )

      :error ->
        []
    end
  end

  defp run_queries(config, distribution_name, dimension, aggregate_variants, attempts \\ 4)

  defp run_queries(_config, _distribution_name, dimension, _aggregate_variants, 0) do
    Logger.log("Giving up. Repeatedly fails: [dimension: #{Utility.name(dimension)}]")
    :error
  end

  defp run_queries(config, distribution_name, dimension, aggregate_variants, attempts) do
    result =
      Enum.flat_map(aggregate_variants, fn aggregate ->
        run_query(config, for_class(distribution_name, dimension, aggregate))
        |> Enum.map(&Map.put(&1, :aggregate, aggregate))
      end)

    {:ok, result}
  rescue
    _e -> run_queries(config, distribution_name, dimension, aggregate_variants, attempts - 1)
  end

  defp run_query(config, query) do
    raw_results =
      query_cloak(config.raw, query)
      |> rows()
      |> to_map()

    anonymized_results =
      config.anonymized
      |> Enum.map(fn dest_config ->
        query_cloak(dest_config, query)
        |> rows()
        |> to_map()
        |> for_destination(dest_config.name)
      end)

    # We bail out if any of the backends returned an empty result set
    if Enum.any?(anonymized_results, &Enum.empty?(elem(&1, 1))) do
      raise "Retry due to empty resultset for backend"
    end

    Enum.flat_map(raw_results, fn {dimension, real_value} ->
      anonymized_results
      |> Enum.map(fn {backend, values} ->
        with anonymized_value when not is_nil(anonymized_value) <- Map.get(values, dimension) do
          %{
            dimension_value: integer_if_possible(dimension),
            source: backend,
            real_value: real_value,
            anonymized_value: anonymized_value,
            error: abs_error(anonymized_value, real_value),
            relative_error: relative_error(anonymized_value, real_value)
          }
        end
      end)
      |> Enum.reject(&is_nil/1)
    end)
  end

  defp abs_error(anonymized_value, real_value), do: abs(anonymized_value - real_value)

  defp relative_error(anonymized_value, 0), do: anonymized_value / @approximation_of_zero
  defp relative_error(anonymized_value, real_value), do: abs_error(anonymized_value, real_value) / real_value

  defp rows(raw_rows), do: Enum.map(raw_rows, & &1["row"])

  defp to_map(rows),
    do:
      rows
      |> Enum.map(fn [dimension, aggregate] -> {dimension, aggregate} end)
      |> Enum.into(%{})

  defp for_destination(data, name), do: {name, data}

  defp query_cloak(config, query) do
    payload = %{
      query: %{
        statement: query,
        data_source_name: config.data_source_name
      }
    }

    http_post(config.host <> "/api/queries", payload, config.api_token, fn result ->
      query_id = result["query_id"]
      await_result(config.host <> "/api/queries/" <> query_id, config.api_token)
    end)
  end

  defp await_result(url, api_token) do
    http_get(url, api_token, fn result ->
      query = result["query"]

      if query["completed"] do
        query["rows"]
      else
        Process.sleep(100)
        await_result(url, api_token)
      end
    end)
  end

  defp http_get(url, api_token, success_callback) do
    headers = [{'auth-token', String.to_charlist(api_token)}]

    case :httpc.request(:get, {String.to_charlist(url), headers}, [], []) do
      {:ok, {{_, 200, _}, _headers, raw_body}} ->
        raw_body |> to_string() |> Poison.decode!() |> success_callback.()

      _result ->
        raise "Unexpected result on GET to #{url}"
    end
  end

  defp http_post(url, payload, api_token, success_callback) do
    headers = [{'auth-token', String.to_charlist(api_token)}]
    content_type = 'application/json'
    json_payload = payload |> Poison.encode!() |> String.to_charlist()

    case :httpc.request(
           :post,
           {String.to_charlist(url), headers, content_type, json_payload},
           [],
           []
         ) do
      {:ok, {{_, 200, _}, _headers, raw_body}} ->
        raw_body |> to_string() |> Poison.decode!() |> success_callback.()

      _result ->
        raise "Unexpected result on POST to #{url}"
    end
  end

  defp for_class(distribution_name, {:dimension, name}, {:count, count}),
    do: """
    SELECT #{name}, #{count}
    FROM data_quality
    WHERE distribution = '#{distribution_name}'
    GROUP BY 1
    """

  defp for_class(distribution_name, {:dimension, name}, aggregate)
       when aggregate in [:min, :max, :avg, :sum],
       do: """
       SELECT #{name}, #{aggregate}(number)
       FROM data_quality
       WHERE distribution = '#{distribution_name}'
       GROUP BY 1
       """

  defp for_class(distribution_name, {:bucket, num}, {:count, count}),
    do: """
    select numBucket, sum(perUserCount) as count FROM (
      SELECT uid, bucket(number by #{num}) as numBucket, #{count} as perUserCount
      FROM data_quality
      WHERE distribution = '#{distribution_name}'
      GROUP BY uid, numBucket
    ) t
    GROUP BY numBucket
    """

  defp for_class(distribution_name, {:bucket, num}, :sum),
    do: """
    select numBucket, sum(perUserSum) as sum FROM (
      SELECT uid, bucket(number by #{num}) as numBucket, sum(number) as perUserSum
      FROM data_quality
      WHERE distribution = '#{distribution_name}'
      GROUP BY uid, numBucket
    ) t
    GROUP BY numBucket
    """

  defp for_class(distribution_name, {:bucket, num}, min_max) when min_max in [:min, :max],
    do: """
    select numBucket, #{min_max}(perUserMinMax) as #{min_max} FROM (
      SELECT uid, bucket(number by #{num}) as numBucket, #{min_max}(number) as perUserMinMax
      FROM data_quality
      WHERE distribution = '#{distribution_name}'
      GROUP BY uid, numBucket
    ) t
    GROUP BY numBucket
    """

  defp for_class(distribution_name, {:bucket, num}, :avg),
    do: """
    select numBucket, sum(perUserSum) / sum(perUserRows) as avg FROM (
      SELECT uid, bucket(number by #{num}) as numBucket, sum(number) as perUserSum, count(*) as perUserRows
      FROM data_quality
      WHERE distribution = '#{distribution_name}'
      GROUP BY uid, numBucket
    ) t
    GROUP BY numBucket
    """

  defp integer_if_possible(nil), do: nil
  defp integer_if_possible(value) when is_number(value), do: value

  defp integer_if_possible(value) do
    cond do
      value =~ ~r/^\d+\.\d+$/ -> String.to_float(value)
      value =~ ~r/^\d+$/ -> String.to_integer(value)
      _otherwise = true -> value
    end
  end
end
