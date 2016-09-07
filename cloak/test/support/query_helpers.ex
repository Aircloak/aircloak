defmodule Cloak.Test.QueryHelpers do
  @moduledoc false

  alias Cloak.Query

  defmacro assert_query(query, expected_response) do
    quote do
      run_query =
        fn(data_source) ->
          Query.Runner.start("1", data_source, unquote(query), {:process, self()})
          receive do
            {:reply, response} -> response
          end
        end

      [first_response | other_responses] =
        Cloak.DataSource.all()
        |> Enum.map(&Task.async(fn -> run_query.(&1) end))
        |> Enum.map(&Task.await/1)

      # make sure responses from all data_sources are equal
      # (ignoring differing execution times)
      first_without_execution_time = Map.drop(first_response, [:execution_time])
      other_responses
      |> Enum.map(&Map.drop(&1, [:execution_time]))
      |> Enum.each(&assert(first_without_execution_time == &1))

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
end
