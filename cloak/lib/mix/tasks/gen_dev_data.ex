if Mix.env == :dev do
  defmodule Mix.Tasks.GenDevData do
    @shortdoc "Generates test data for dev database."
    @moduledoc false

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    use Mix.Task

    @doc false
    def run(_args), do:
      [:postgresql, :saphana]
      |> Enum.map(&{&1, open_connection(&1)})
      |> Enum.filter(fn({_adapter, connect_result}) -> match?({:ok, _conn}, connect_result) end)
      |> Enum.map(fn({adapter, {:ok, conn}}) -> {adapter, conn} end)
      |> Enum.each(fn({adapter, _} = descriptor) ->
        IO.puts "importing to #{adapter}"
        insert(descriptor, integers_data())
        IO.puts "done\n"
      end)

    defp integers_data() do
      %{
        name: "integers",
        columns: [{"user_id", "integer"}, {"value", "integer"}],
        data:
          Stream.flat_map(
            1..2500,
            fn(value) -> Stream.map(1..10, &[&1, value]) end
          )
      }
    end

    defp insert({:postgresql, conn}, table_spec) do
      Postgrex.query!(conn, "DROP TABLE IF EXISTS #{table_spec.name}", [])

      Postgrex.query!(conn, create_statement(table_spec), [])

      table_spec.data
      |> Stream.map(&"(#{Enum.join(&1, ", ")})")
      |> Stream.chunk(10, 10, [])
      |> Stream.map(&Enum.join(&1, ","))
      |> Enum.each(
        fn(chunk_sql) ->
          Postgrex.query!(
            conn,
            [
              "
                INSERT INTO #{table_spec.name} (#{table_spec.columns |> Enum.map(&elem(&1, 0)) |> Enum.join(", ")})
                VALUES #{chunk_sql}
              "
            ],
            []
          )
        end
      )
    end
    defp insert({:saphana, conn}, table_spec) do
      {:selected, _, rows} = :odbc.sql_query(conn,
        'SELECT table_name FROM tables where table_name=\'#{String.upcase(table_spec.name)}\'')

      if length(rows) == 1, do:
        {:updated, _} = :odbc.sql_query(conn, 'DROP TABLE #{table_spec.name}')

      {:updated, _} = :odbc.sql_query(conn, table_spec |> create_statement() |> to_char_list())

      chunks =
        table_spec.data
        |> Stream.map(&'SELECT #{Enum.join(&1, ", ")} from dummy')
        |> Stream.chunk(1000, 1000, [])
        |> Enum.map(&'(#{Enum.join(&1, " UNION ALL ")})')

      chunks
      |> Enum.with_index()
      |> Enum.each(fn({chunk_sql, index}) ->
        IO.puts "chunk #{index+1}/#{length(chunks)}"
        {:updated, _} =
          :odbc.sql_query(
            conn,
            '
              INSERT INTO #{table_spec.name}(#{table_spec.columns |> Enum.map(&elem(&1, 0)) |> Enum.join(", ")})
              #{chunk_sql}
            '
          )
      end)
    end

    defp open_connection(:postgresql) do
      Application.ensure_all_started(:postgrex)
      db_params =
        Aircloak.DeployConfig.fetch!(:cloak, "data_sources")
        |> Enum.find(&(&1["name"] == "cloak_postgres_native"))
        |> Map.fetch!("parameters")

      Postgrex.start_link(
        database: db_params["database"],
        hostname: db_params["hostname"],
        username: db_params["username"],
        password: db_params["password"]
      )
    end
    defp open_connection(:saphana) do
      Application.ensure_all_started(:odbc)

      db_params =
        Aircloak.DeployConfig.fetch!(:cloak, "data_sources")
        |> Enum.find(&(&1["name"] == "saphana"))
        |> Map.fetch!("parameters")

      [
        "DSN=SAPHANA",
        "servernode=#{Map.fetch!(db_params, "hostname")}:#{Map.fetch!(db_params, "port")}",
        "uid=#{Map.fetch!(db_params, "username")}",
        "pwd=#{Map.fetch!(db_params, "password")}",
        "databasename=#{Map.fetch!(db_params, "database")}"
      ]
      |> Enum.join(";")
      |> to_char_list()
      |> :odbc.connect(auto_commit: :on, binary_strings: :on, tuple_row: :off)
    end

    defp create_statement(table_spec), do:
    "
      CREATE TABLE #{table_spec.name} (
        #{
          table_spec.columns
          |> Enum.map(fn({name, type}) -> "#{name} #{type}" end)
          |> Enum.join(", ")
        }
      )
    "
  end
end
