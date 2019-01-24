defmodule Compliance.MaterializedViewTest do
  use ComplianceCase, async: true
  alias Cloak.MaterializedView

  @moduletag :compliance
  @tested_data_sources ~w(oracle postgresql9.4 postgresql)

  setup do
    for data_source <- tested_data_sources(),
        table_name <- MaterializedView.stored(data_source),
        quote_char = data_source.driver.sql_dialect_module.quote_char(),
        quoted_table_name = Cloak.DataSource.SqlBuilder.quote_table_name(table_name, quote_char),
        do: execute!(data_source, "drop table #{quoted_table_name}")

    :ok
  end

  for data_source_name <- @tested_data_sources do
    describe "#{data_source_name}" do
      test "all returns an empty list when there are no views" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)),
             do: assert(MaterializedView.stored(data_source) == [])
      end

      test "all returns the list of the created views" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, view1} = MaterializedView.new(1, "select user_id, height from users where age < 70", data_source)
          :ok = MaterializedView.store(view1)

          {:ok, view2} = MaterializedView.new(2, "select user_id, height from users where age < 70", data_source)
          :ok = MaterializedView.store(view2)

          assert Enum.sort(MaterializedView.stored(data_source)) ==
                   [view1, view2] |> Enum.map(&MaterializedView.name/1) |> Enum.sort()
        end
      end

      test "view can be created" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, view} = MaterializedView.new(1, "select user_id, height from users where age < 70", data_source)
          assert MaterializedView.store(view) == :ok
        end
      end

      test "view can be created multiple times" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, view} = MaterializedView.new(1, "select user_id, height from users where age < 70", data_source)
          assert MaterializedView.store(view) == :ok
          assert MaterializedView.store(view) == :ok
        end
      end

      test "stored view contains desired rows" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, view} = MaterializedView.new(1, "select user_id, height from users where age < 70", data_source)
          :ok = MaterializedView.store(view)

          materialized = select!(data_source, "select * from #{quote_view_name(view)}")
          expected = select_direct!(data_source, "select user_id, height from users where age < 70")
          assert materialized == expected
        end
      end
    end
  end

  defp tested_data_sources(), do: Enum.filter(Cloak.DataSource.all(), &(&1.name in @tested_data_sources))

  defp execute!(data_source, statement) do
    Cloak.DataSource.Connection.execute!(
      data_source,
      fn connection ->
        case data_source.driver do
          Cloak.DataSource.PostgreSQL -> Postgrex.query!(connection, statement, [])
          Cloak.DataSource.Oracle -> Cloak.DataSource.RODBC.select_direct!(connection, statement)
        end
      end
    )
  end

  defp select!(data_source, statement) do
    case data_source.driver do
      Cloak.DataSource.PostgreSQL -> execute!(data_source, statement).rows
      Cloak.DataSource.Oracle -> Enum.to_list(execute!(data_source, statement))
    end
  end

  defp select_direct!(data_source, statement) do
    parsed = Cloak.Sql.Parser.parse!(statement)
    compiled = Cloak.Sql.Compiler.compile_direct!(parsed, data_source)
    Cloak.Query.DbEmulator.select(compiled)
  end

  defp quote_view_name(%MaterializedView{} = view), do: quote_table_name(view.data_source, MaterializedView.name(view))

  defp quote_table_name(data_source, name) do
    quote_char = data_source.driver.sql_dialect_module.quote_char()
    Cloak.DataSource.SqlBuilder.quote_table_name(name, quote_char)
  end
end
