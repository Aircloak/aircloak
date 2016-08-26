defmodule Cloak.Test.QueryHelpers do
  @moduledoc false

  alias Cloak.Query

  defmacro assert_query(query, expected_response) do
    quote do
      [first_ds | rest_ds] = Cloak.DataSource.all()
      :ok = start_query(unquote(query), first_ds)
      assert_receive {:reply, response}, 1000
      for next_ds <- rest_ds do
        :ok = start_query(unquote(query), next_ds)
        assert_receive {:reply, ^response}, 1000
      end
      assert unquote(expected_response) = response
    end
  end

  defmacro assert_info(query, expected_info_regex) do
    quote do
      assert_query unquote(query), %{info: [info]}
      assert info =~ unquote(expected_info_regex)
    end
  end

  def start_query(statement, data_source) do
    Query.Runner.start("1", data_source, statement, {:process, self()})
  end

  def insert_rows(user_id_range, table, columns, values) do
    Cloak.Test.DB.add_users_data(table, columns, Enum.map(user_id_range, &["user#{&1}" | values]))
  end
end
