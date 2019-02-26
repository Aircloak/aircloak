defmodule Cloak.Test.AnalystTableHelpers do
  @moduledoc false

  alias Cloak.AnalystTable
  import Aircloak.AssertionHelper

  def stored_tables(data_source),
    do: Cloak.DataSource.Connection.execute!(data_source, &data_source.driver.analyst_tables/1)

  def clear_analyst_tables(data_source) do
    AnalystTable.sync_serialized(fn ->
      data_source |> stored_tables() |> Enum.each(&drop_table!(data_source, &1))
      truncate_table!(data_source, "__ac_analyst_tables_1")
      :ets.match_delete(AnalystTable, {{:_, :_, data_source.name, :_}, :_})
    end)
  end

  def create_or_update(analyst_id, name, statement, data_source) do
    with {:ok, columns} <- AnalystTable.create_or_update(analyst_id, name, statement, data_source) do
      true = soon(table_created?(analyst_id, name, data_source), :timer.seconds(5), repeat_wait_time: 10)
      {:ok, columns}
    end
  end

  def table_created?(analyst_id, name, data_source, expected_status \\ :created) do
    with table_definition <- AnalystTable.find(analyst_id, name, data_source),
         false <- is_nil(table_definition),
         ^expected_status <- table_definition.status,
         do: true,
         else: (_ -> false)
  end

  defp drop_table!(data_source, table_name) do
    quote_char = data_source.driver.sql_dialect_module.quote_char()
    quoted_table_name = Cloak.DataSource.SqlBuilder.quote_table_name(table_name, quote_char)
    execute!(data_source, "drop table #{quoted_table_name}")
  end

  defp truncate_table!(data_source, table_name) do
    quote_char = data_source.driver.sql_dialect_module.quote_char()
    quoted_table_name = Cloak.DataSource.SqlBuilder.quote_table_name(table_name, quote_char)
    execute!(data_source, "truncate table #{quoted_table_name}")
  end

  defp execute!(data_source, statement) do
    Cloak.DataSource.Connection.execute!(
      data_source,
      fn connection ->
        case data_source.driver do
          Cloak.DataSource.PostgreSQL -> Postgrex.query!(connection, statement, [])
          Cloak.DataSource.Oracle -> Cloak.DataSource.RODBC.execute_direct!(connection, statement)
        end
      end
    )
  end
end
