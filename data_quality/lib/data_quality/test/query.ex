defmodule DataQuality.Test.Query do
  @moduledoc "Executes queries and collects query results for further processing"

  alias Aircloak.OutputStatus
  alias DataQuality.Distributions
  alias DataQuality.Test
  alias DataQuality.Test.{Utility, Logger}

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
    name = test[:name]
    aggregates = test[:aggregates]

    Logger.header(name)

    Distributions.list()
    |> Enum.flat_map(fn distribution ->
      distribution_name = Distributions.distribution_name(distribution)
      OutputStatus.new_line(distribution_name, :pending, "querying")

      results =
        dimensions
        |> Enum.flat_map(fn dimension ->
          test_dimension(config, distribution_name, dimension, aggregates)
          |> Enum.map(
            &Map.merge(&1, %{
              dimension: dimension,
              distribution: distribution_name,
              class: name
            })
          )
        end)

      OutputStatus.done(distribution_name)
      results
    end)
  end

  defp test_dimension(config, distribution_name, dimension, aggregate_variants, attempts \\ 4)

  defp test_dimension(_config, _distribution_name, dimension, _aggregate_variants, 0) do
    Logger.log("Giving up. Repeatedly fails: [dimension: #{Utility.name(dimension)}]")
    []
  end

  defp test_dimension(config, distribution_name, dimension, aggregate_variants, attempts) do
    Enum.flat_map(aggregate_variants, fn aggregate ->
      results = generate_results(config, for_class(distribution_name, dimension, aggregate))
      Enum.map(results, &Map.put(&1, :aggregate, aggregate))
    end)
  rescue
    _e -> test_dimension(config, distribution_name, dimension, aggregate_variants, attempts - 1)
  end

  defp generate_results(config, query) do
    raw_results =
      run_query(config.raw, query)
      |> rows()
      |> to_map()

    anonymized_results =
      config.anonymized
      |> Enum.map(fn dest_config ->
        run_query(dest_config, query)
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
        case Map.get(values, dimension) do
          nil ->
            nil

          anonymized_value ->
            %{
              source: backend,
              real_value: real_value,
              anonymized_value: anonymized_value,
              error: abs(real_value - anonymized_value),
              relative_error: abs(anonymized_value - real_value) / real_value
            }
        end
      end)
      |> Enum.reject(&is_nil/1)
    end)
  end

  defp rows(raw_rows), do: Enum.map(raw_rows, & &1["row"])

  defp to_map(rows),
    do:
      rows
      |> Enum.map(fn [dimension, aggregate] -> {dimension, aggregate} end)
      |> Enum.into(%{})

  defp for_destination(data, name), do: {name, data}

  defp run_query(config, query) do
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
end
