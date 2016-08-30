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
      Enum.each(other_responses, &assert(first_response == &1))

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
