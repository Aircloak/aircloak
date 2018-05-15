defmodule Cloak.Test.QueryHelpers do
  @moduledoc false

  alias Cloak.Sql.{Compiler, Parser, Query, Expression}

  defmacro assert_query_consistency(query, options \\ []) do
    quote bind_quoted: [
            query: query,
            parameters: Keyword.get(options, :parameters, []),
            views: Keyword.get(options, :views, quote(do: %{})),
            data_sources: Keyword.get(options, :data_sources, quote(do: Cloak.DataSource.all())),
            timeout: Keyword.get(options, :timeout, :timer.hours(1))
          ] do
      run_query = &Cloak.Query.Runner.run_sync("1", &1, query, parameters, views)

      results =
        data_sources
        |> Task.async_stream(
          fn data_source ->
            {us, result} = :timer.tc(fn -> run_query.(data_source) end)
            Compliance.Runtime.record(data_source, _ms = div(us, 1000))
            result
          end,
          timeout: timeout,
          on_timeout: :kill_task
        )
        |> Stream.zip(data_sources)
        |> Enum.map(fn
          {{:ok, result}, data_source} ->
            {data_source, Map.drop(result, [:execution_time, :features])}

          {{:exit, _} = exit_value, data_source} ->
            {data_source, exit_value}
        end)

      case Enum.reduce(results, [], &Cloak.Test.QueryHelpers.append_result(&2, &1)) do
        [[{_first_data_source, first_result} | _]] ->
          first_result

        multiple_groups ->
          # Note: we report only first two groups, because we can then use `left` and `right` fields in assertion error,
          # which gives us a nicer looking diff output. We'll take the two most populated groups to maximize the number
          # of reported datasources.
          [group1, group2 | _] = Enum.sort_by(multiple_groups, &length/1, &>=/2)

          {datasources1, [result1 | _]} = Enum.unzip(group1)
          {datasources2, [result2 | _]} = Enum.unzip(group2)

          datasources1 = datasources1 |> Enum.sort() |> Enum.map(&"  #{&1}\n")
          datasources2 = datasources2 |> Enum.sort() |> Enum.map(&"  #{&1}\n")

          raise ExUnit.AssertionError,
            message:
              "Inconsistent query results:\n" <>
                "Group 1:\n#{datasources1}" <> "Group 2:\n#{datasources2}" <> "Query:\n#{query}",
            left: result1,
            right: result2
      end
    end
  end

  def append_result([], {data_source, result}), do: [[{name_datasource(data_source), result}]]

  def append_result([group | other_groups], {data_source, result}) do
    if Enum.any?(group, &same_result?(&1, data_source, result)) do
      [[{name_datasource(data_source), result} | group] | other_groups]
    else
      [group | append_result(other_groups, {data_source, result})]
    end
  end

  defmacro assert_query(query, options \\ [], expected_response) do
    quote do
      assert unquote(expected_response) = assert_query_consistency(unquote(query), unquote(options))
    end
  end

  defmacro assert_info(query, expected_info_regex) do
    quote do
      assert_query(unquote(query), %{info: [info]})
      assert info =~ unquote(expected_info_regex)
    end
  end

  def insert_rows(user_id_range, table, columns, values) do
    Cloak.Test.DB.add_users_data(table, columns, Enum.map(user_id_range, &["user#{&1}" | values]))
  end

  def insert_null_uid_row(table, columns, values) do
    Cloak.Test.DB.add_users_data(table, columns, [[nil | values]])
  end

  defp only_structs_lens(root), do: Lens.filter(root, &is_map/1)

  defp tables_lens(),
    do:
      Lens.both(
        Lens.key(:selected_tables) |> Lens.all(),
        Query.Lenses.query_expressions() |> Lens.key(:table) |> only_structs_lens()
      )

  defp virtual_queries_lens(), do: tables_lens() |> Lens.key(:query) |> only_structs_lens()

  defp data_sources_lens(query_lens),
    do:
      query_lens
      |> Lens.both(Lens.root(), virtual_queries_lens())
      |> Lens.key(:data_source)

  def scrub_data_sources(query), do: put_in(query, [Query.Lenses.all_queries() |> data_sources_lens()], nil)

  def scrub_locations(ast),
    do:
      update_in(ast, [Query.Lenses.all_queries() |> Query.Lenses.terminals()], fn
        {:identifier, table, column, _location} ->
          {:identifier, table, column, nil}

        {:constant, type, value, _location} ->
          {:constant, type, value, nil}

        {:like_pattern, {:constant, type1, value1, _loc1}, {:constant, type2, value2, _loc2}} ->
          {:like_pattern, {:constant, type1, value1, nil}, {:constant, type2, value2, nil}}

        {:function, name, arguments, _location} ->
          {:function, name, arguments, nil}

        expression = %Expression{} ->
          %{expression | source_location: nil}

        other ->
          other
      end)

  def compile!(query_string, data_source, options \\ []) do
    {:ok, result} = compile(query_string, data_source, options)
    result
  end

  def compile(query_string, data_source, options \\ []) do
    with {:ok, parsed_query} <- Parser.parse(query_string),
         {:ok, query} <-
           Compiler.compile(
             parsed_query,
             data_source,
             Keyword.get(options, :parameters, []),
             Keyword.get(options, :views, %{})
           ) do
      {:ok, Query.resolve_db_columns(query)}
    end
  end

  defp same_result?({ds_name, existing_result}, data_source, result) do
    deltas = %{float: 0.000001, user_count: user_count_delta(ds_name, data_source)}
    compare_to_within_delta(existing_result, result, ["root"], deltas) == :ok
  end

  defp user_count_delta(ds_name, data_source) do
    if Enum.member?([ds_name, name_datasource(data_source)], "'Cloak.DataSource.SAPIQ/sapiq/sapiq'") do
      # There is a strange issue with SAP IQ and rodbc. In some cases we get one row less from the database, which
      # causes compliance mismatch. I presume that this is a bug in Rust's ODBC, because Erlang's :odbc works properly.
      # Since that one row shouldn't play a significant role in the anonymized output, I opted for this hacky
      # workaround where we tolerate a mismatch of one in `:users_count` and `:occurrences` fields.
      1
    else
      0
    end
  end

  defp compare_to_within_delta(
         %{occurrences: o1, users_count: uc1} = map1,
         %{occurrences: o2, users_count: uc2} = map2,
         trace,
         deltas
       )
       when uc1 != nil and uc2 != nil do
    if abs(o1 - o2) <= deltas.user_count and abs(uc1 - uc2) <= deltas.user_count do
      keys_to_drop = [:occurrences, :users_count]
      compare_to_within_delta(Map.drop(map1, keys_to_drop), Map.drop(map2, keys_to_drop), trace, deltas)
    else
      {:error, trace}
    end
  end

  defp compare_to_within_delta(map1, map2, trace, deltas) when is_map(map1) and is_map(map2) do
    if Map.keys(map1) == Map.keys(map2) do
      Map.keys(map1)
      |> Enum.reduce(:ok, fn
        key, :ok ->
          compare_to_within_delta(Map.get(map1, key), Map.get(map2, key), [key | trace], deltas)

        _, error ->
          error
      end)
    else
      {:error, trace}
    end
  end

  defp compare_to_within_delta(list1, list2, trace, deltas)
       when is_list(list1) and is_list(list2) do
    if length(list1) == length(list2) do
      Enum.zip(list1, list2)
      |> Enum.with_index()
      |> Enum.reduce(:ok, fn
        {{value1, value2}, index}, :ok ->
          compare_to_within_delta(value1, value2, ["##{index}" | trace], deltas)

        _, error ->
          error
      end)
    else
      {:error, trace}
    end
  end

  defp compare_to_within_delta(value1, value2, trace, deltas)
       when is_float(value1) and is_float(value2) do
    magnitude = abs((value1 + value2) / 2)
    diff = abs(value1 - value2) / max(magnitude, deltas.float)

    if diff <= deltas.float do
      :ok
    else
      {:error, trace}
    end
  end

  defp compare_to_within_delta(value1, value2, trace, _deltas) do
    if value1 == value2 do
      :ok
    else
      {:error, trace}
    end
  end

  defp name_datasource(data_source),
    do: "'#{inspect(data_source.driver)}/#{sql_dialect_name(data_source)}/#{data_source.name}'"

  defp sql_dialect_name(data_source) do
    case Cloak.DataSource.sql_dialect_module(data_source) do
      nil ->
        nil

      dialect_module ->
        dialect_module
        |> to_string()
        |> String.split(".")
        |> List.last()
        |> String.downcase()
    end
  end
end
