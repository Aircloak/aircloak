defmodule Cloak.Test.DB do
  alias Cloak.DataSource
  alias Cloak.DataSource.PostgreSQL

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def clear_table(db_name) do
    PostgreSQL.execute("TRUNCATE TABLE #{sanitized_table(db_name)}", [])
  end

  def create_table(table_name, definition, opts \\ []) do
    GenServer.call(__MODULE__, {:create_table, table_name, definition, opts})
  end

  def add_users_data(table_name, columns, rows) do
    row_count = length(rows)
    {sql, params} = prepare_insert(table_name, columns, rows)
    {:ok, %Postgrex.Result{num_rows: ^row_count}} = PostgreSQL.execute(sql, params)
    :ok
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  def init(_) do
    PostgreSQL.execute("DROP SCHEMA IF EXISTS cloak_test CASCADE", [])
    DataSource.clear_test_tables()
    PostgreSQL.execute("CREATE SCHEMA cloak_test", [])
    {:ok, nil}
  end

  def handle_call({:create_table, table_name, definition, opts}, _from, state) do
    db_name = opts[:db_name] || table_name
    case create_db_table(db_name, definition, opts) do
      {:ok, _} ->
        DataSource.register_test_table(String.to_atom(table_name), full_table_name(db_name), "user_id")
        {:reply, :ok, state}
      error ->
        {:reply, error, state}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp create_db_table(db_name, definition, opts) do
    if opts[:skip_db_create] do
      {:ok, :already_created}
    else
      PostgreSQL.execute(
        "CREATE TABLE #{sanitized_table(db_name)} (user_id VARCHAR(64), #{definition})",
        []
      )
    end
  end

  defp prepare_insert(table_name, columns, rows) do
    columns = Enum.map(["user_id" | columns], &sanitize_db_object/1)

    n_columns = length(columns)
    value_tuples =
      for {row, row_index} <- Enum.with_index(rows) do
        params_string =
          for {_column, column_index} <- Enum.with_index(row) do
            ["$#{row_index * n_columns + column_index + 1}"]
          end
          |> Enum.join(",")

        "(#{params_string})"
      end
      |> Enum.join(",")

    {
      "INSERT INTO #{sanitized_table(table_name)}(#{Enum.join(columns, ",")}) VALUES #{value_tuples}",
      Enum.flat_map(rows, &(&1))
    }
  end

  defp sanitized_table(table_name), do: sanitize_db_object(full_table_name(table_name))

  defp sanitize_db_object(db_object) do
    for part <- Regex.split(~r/\./, db_object) do
      "\"#{Regex.replace(~r/"/, part, "\"\"")}\""
    end
    |> Enum.join(".")
  end

  defp full_table_name(table_name), do: "cloak_test.#{table_name}"
end
