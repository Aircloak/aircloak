defmodule Cloak.Test.DB do
  @moduledoc false

  # This module is used only for test purposes. Since it is also used in integration_tests
  # project, it needs to reside in lib folder.

  alias Cloak.DataSource

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def clear_table(db_name) do
    execute!("DELETE FROM #{sanitized_table(db_name)}")
  end

  def create_table(table_name, definition, opts \\ []) do
    GenServer.call(__MODULE__, {:create_table, table_name, definition, opts}, :infinity)
  end

  def add_users_data(table_name, columns, rows), do:
    insert_data(table_name, ["user_id" | columns], rows)

  def insert_data(table_name, columns, rows) do
    {sql, params} = prepare_insert(table_name, columns, rows)
    execute!(sql, params)
  end

  def execute!(statement, parameters \\ []) do
    for data_source <- DataSource.all() do
      if data_source.driver.__info__(:functions)[:execute] do # check if driver supports direct query execution
        connection = Process.get({:connection, data_source.global_id}) || create_connection(data_source)
        {:ok, _result} = data_source.driver.execute(connection, statement, parameters)
      end
    end
    :ok
  end

  def register_test_table(table_id, db_name, opts \\ []) do
    default = %{
      db_name: db_name,
      user_id: (if Keyword.get(opts, :add_user_id, true), do: "user_id", else: nil),
      decoders: [],
      projection: nil
    }
    user_overrides =
      opts
      |> Enum.into(%{})
      |> Map.take([:user_id, :decoders, :projection])
    table = Map.merge(default, user_overrides)

    DataSource.all()
    |> Enum.map(&(&1 |> put_in([:initial_tables, table_id], table) |> DataSource.add_tables()))
    |> DataSource.update()
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  def init(_) do
    execute!("DROP SCHEMA IF EXISTS cloak_test CASCADE")
    execute!("CREATE SCHEMA cloak_test")
    {:ok, nil}
  end

  def handle_call({:create_table, table_name, definition, opts}, _from, state) do
    db_name = opts[:db_name] || table_name
    create_db_table(db_name, definition, opts)
    register_test_table(String.to_atom(table_name), full_table_name(db_name), opts)
    {:reply, :ok, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp create_db_table(db_name, definition, opts) do
    if opts[:skip_db_create] do
      :ok
    else
      user_id_column = if Keyword.get(opts, :add_user_id, true), do: "user_id VARCHAR(64),", else: ""
      execute!("CREATE TABLE #{sanitized_table(db_name)} (#{user_id_column} #{definition})")
    end
  end

  defp prepare_insert(table_name, columns, rows) do
    {
      [
        "INSERT INTO ", sanitized_table(table_name),
          "( ", Enum.join(columns, ","), ") ",
          "VALUES ", rows |> rows_tuples(length(columns)) |> Enum.join(",")
      ],
      Enum.flat_map(rows, &(&1))
    }
  end

  defp rows_tuples(rows, num_columns) do
    for {row, row_index} <- Enum.with_index(rows) do
      [?(, Enum.join(row_params(row, row_index * num_columns), ","), ?)]
    end
  end

  defp row_params(row, row_offset) do
    for {_column, column_index} <- Enum.with_index(row), do: "$#{row_offset + column_index + 1}"
  end

  defp sanitized_table(table_name), do: sanitize_db_object(full_table_name(table_name))

  defp sanitize_db_object(db_object) do
    for part <- Regex.split(~r/\./, db_object) do
      "\"#{Regex.replace(~r/"/, part, "\"\"")}\""
    end
    |> Enum.join(".")
  end

  defp full_table_name(table_name), do: "cloak_test.#{table_name}"

  defp create_connection(data_source) do
    connection = data_source.driver.connect!(data_source.parameters)
    Process.put({:connection, data_source.global_id}, connection)
    connection
  end
end
