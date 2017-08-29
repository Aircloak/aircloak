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
        assert(first_response == other_response)

      first_response
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
