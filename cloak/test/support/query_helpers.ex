defmodule Cloak.Test.QueryHelpers do
  @moduledoc false

  alias Cloak.Sql.{Compiler, Parser, Query}

  defmacro assert_query_consistency(query, options \\ []) do
    quote bind_quoted: [
      query: query,
      parameters: Keyword.get(options, :parameters, []),
      views: Keyword.get(options, :views, quote(do: %{})),
      data_sources: Keyword.get(options, :data_sources, quote(do: Cloak.DataSource.all())),
      timeout: Keyword.get(options, :timeout, :timer.minutes(15))
    ] do
      run_query = &Cloak.Query.Runner.run_sync("1", &1, query, parameters, views)

      [{first_data_source, first_result} | other_results] = data_sources
      |> Task.async_stream(fn(data_source) ->
        {us, result} = :timer.tc(fn() -> run_query.(data_source) end)
        Compliance.Runtime.record(data_source, _ms = div(us, 1000))
        result
      end, timeout: timeout, on_timeout: :kill_task)
      |> Stream.zip(data_sources)
      |> Enum.map(fn
        ({{:ok, result}, data_source}) -> {data_source, Map.drop(result, [:execution_time, :features])}
        ({{:exit, _} = exit_value, data_source}) -> {data_source, exit_value}
      end)

      for {other_data_source, other_result} <- other_results, do:
        Cloak.Test.QueryHelpers.assert_equal(first_result, other_result, 0.000001,
          first_data_source, other_data_source, query)

      first_result
    end
  end

  def assert_equal(value1, value2, delta, data_source1, data_source2, query) do
    case compare_to_within_delta(value1, value2, ["root"], delta) do
      :ok -> true
      {:error, trace} ->
        raise ExUnit.AssertionError,
          message: "Comparison failed at #{trace |> Enum.reverse() |> Enum.join(" > ")} while comparing " <>
            "results from #{name_datasource(data_source1)} with results from " <>
            name_datasource(data_source2) <> ". Query was: \n#{query}.",
          left: value1,
          right: value2
    end
  end

  defmacro assert_query(query, options \\ [], expected_response) do
    quote do
      assert unquote(expected_response) = assert_query_consistency(unquote(query), unquote(options))
    end
  end

  defmacro assert_info(query, expected_info_regex) do
    quote do
      assert_query unquote(query), %{info: [info]}
      assert info =~ unquote(expected_info_regex)
    end
  end

  def insert_rows(user_id_range, table, columns, values) do
    Cloak.Test.DB.add_users_data(table, columns, Enum.map(user_id_range, &["user#{&1}" | values]))
  end

  def insert_null_uid_row(table, columns, values) do
    Cloak.Test.DB.add_users_data(table, columns, [[nil | values]])
  end

  def scrub_data_sources(query), do:
    put_in(query, [Query.Lenses.all_queries() |> Lens.key(:data_source)], nil)

  def scrub_locations(ast), do:
    update_in(ast, [Query.Lenses.all_queries() |> Query.Lenses.terminals()], fn
      {:identifier, table, column, _location} -> {:identifier, table, column, nil}
      {:constant, type, value, _location} -> {:constant, type, value, nil}
      other -> other
    end)

  def compile!(query_string, data_source, options \\ []) do
    {:ok, result} = compile(query_string, data_source, options)
    result
  end

  def compile(query_string, data_source, options \\ []) do
    with \
      {:ok, parsed_query} <- Parser.parse(query_string),
      {:ok, query, _features} <-
        Compiler.compile(
          data_source,
          parsed_query,
          Keyword.get(options, :parameters, []),
          Keyword.get(options, :views, %{})
        )
    do
      {:ok, Query.resolve_db_columns(query)}
    end
  end

  defp compare_to_within_delta(map1, map2, trace, delta) when is_map(map1) and is_map(map2) do
    if Map.keys(map1) == Map.keys(map2) do
      Map.keys(map1)
      |> Enum.reduce(:ok, fn
        (key, :ok) ->
          compare_to_within_delta(Map.get(map1, key), Map.get(map2, key), [key | trace], delta)
        (_, error) -> error
      end)
    else
      {:error, trace}
    end
  end
  defp compare_to_within_delta(list1, list2, trace, delta) when is_list(list1) and is_list(list2) do
    if length(list1) == length(list2) do
      Enum.zip(list1, list2)
      |> Enum.with_index()
      |> Enum.reduce(:ok, fn
        ({{value1, value2}, index}, :ok) ->
          compare_to_within_delta(value1, value2, ["##{index}" | trace], delta)
        (_, error) -> error
      end)
    else
      {:error, trace}
    end
  end
  defp compare_to_within_delta(value1, value2, trace, delta) when is_float(value1) and is_float(value2) do
    magnitude = abs((value1 + value2) / 2)
    diff = abs(value1 - value2) / max(magnitude, delta)
    if diff <= delta do
      :ok
    else
      {:error, trace}
    end
  end
  defp compare_to_within_delta(value1, value2, trace, _delta) do
    if value1 == value2 do
      :ok
    else
      {:error, trace}
    end
  end

  defp name_datasource(data_source), do:
    "'#{inspect(data_source.driver)}/#{sql_dialect_name(data_source)}/#{data_source.name}'"

  defp sql_dialect_name(data_source) do
    case  Cloak.DataSource.sql_dialect_module(data_source) do
      nil -> nil

      dialect_module ->
        dialect_module
        |> to_string()
        |> String.split(".")
        |> List.last()
        |> String.downcase()
    end
  end
end
