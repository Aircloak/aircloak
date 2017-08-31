defmodule Cloak.Test.QueryHelpers do
  @moduledoc false

  import Lens.Macros

  alias Cloak.Sql.{Expression, Compiler, Parser, Query}

  defmacro assert_query_consistency(query, options \\ []) do
    parameters = Keyword.get(options, :parameters, [])
    views = Keyword.get(options, :views, quote(do: %{}))
    quote do
      run_query =
        fn(data_source) ->
          Cloak.Query.Runner.start("1", data_source, unquote(query), unquote(parameters), unquote(views),
            {:process, self()})
          receive do
            {:result, response} -> response
          end
        end

      [first_response | other_responses] =
        Cloak.DataSource.all()
        |> Enum.map(&Task.async(fn -> run_query.(&1) end))
        |> Enum.map(&Task.await/1)
        |> Enum.map(&Map.drop(&1, [:execution_time, :features]))

      for other_response <- other_responses, do:
        assert_equal_to_within_delta(first_response, other_response, 0.00000001)

      first_response
    end
  end

  def assert_equal_to_within_delta(value1, value2, delta) do
    case compare_to_within_delta(value1, value2, ["root"], delta) do
      :ok -> true
      {:error, trace} ->
        raise ExUnit.AssertionError,
          message: "Comparison failed at #{trace |> Enum.reverse() |> Enum.join(" > ")}.",
          left: value1,
          right: value2
    end
  end

  def compare_to_within_delta(map1, map2, trace, delta) when is_map(map1) and is_map(map2) do
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
  def compare_to_within_delta(list1, list2, trace, delta) when is_list(list1) and is_list(list2) do
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
  def compare_to_within_delta(value1, value2, trace, delta) when is_float(value1) and is_float(value2) do
    diff = abs(value1 - value2)
    if diff <= delta do
      :ok
    else
      {:error, trace}
    end
  end
  def compare_to_within_delta(value1, value2, trace, _delta) do
    if value1 == value2 do
      :ok
    else
      {:error, trace}
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

  deflens aliases, do:
    all_subqueries() |> Query.Lenses.terminals() |> Lens.satisfy(&match?(%Expression{}, &1)) |> Lens.key(:alias)

  deflens all_subqueries(), do:
    Lens.both(Lens.recur(Query.Lenses.direct_subqueries() |> Lens.key(:ast)), Lens.root())

  def scrub_data_sources(query), do:
    put_in(query, [all_subqueries() |> Lens.key(:data_source)], nil)

  def scrub_aliases(query), do: put_in(query, [aliases()], nil)

  def compile!(query_string, data_source, options \\ []) do
    {:ok, result} = compile(query_string, data_source, options)
    result
  end

  def compile(query_string, data_source, options \\ []) do
    with {:ok, parsed_query} <- Parser.parse(query_string), do:
      Compiler.compile(
        data_source,
        parsed_query,
        Keyword.get(options, :parameters, []),
        Keyword.get(options, :views, %{})
        )
  end
end
