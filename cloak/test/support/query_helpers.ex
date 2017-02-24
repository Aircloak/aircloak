defmodule Cloak.Test.QueryHelpers do
  @moduledoc false

  alias Cloak.Query

  defmacro assert_query(query, options \\ [], expected_response) do
    parameters = Keyword.get(options, :parameters, [])
    views = Keyword.get(options, :views, quote(do: %{}))
    quote do
      run_query =
        fn(data_source) ->
          Query.Runner.start("1", data_source, unquote(query), unquote(parameters), unquote(views),
            {:process, self()})
          receive do
            {:result, response} -> response
          end
        end

      [first_response | other_responses] =
        Cloak.DataSource.all()
        |> Enum.map(&Task.async(fn -> run_query.(&1) end))
        |> Enum.map(&Task.await/1)
        # ignore differing execution times and data sources not supporting this query
        |> Enum.map(&Map.drop(&1, [:execution_time]))
        |> Enum.reject(&Regex.match?(~r/not supported on '[\w\d\s]+' data sources\.$/, Map.get(&1, :error, "")))

      # make sure responses from all data_sources are equal
      for other_response <- other_responses, do: assert(first_response == other_response)

      assert unquote(expected_response) = first_response
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
end
