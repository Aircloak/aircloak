defmodule Cloak.Test.DB do
  alias Cloak.DataSource
  alias Cloak.DataSource.PostgreSQL

  def setup do
    PostgreSQL.execute("DROP SCHEMA IF EXISTS cloak_test CASCADE", [])
    DataSource.clear_test_tables()
  end

  def create_test_schema do
    PostgreSQL.execute("CREATE SCHEMA cloak_test", [])
  end

  def clear_table(db_name) do
    PostgreSQL.execute("TRUNCATE TABLE #{sanitized_table(db_name)}", [])
  end

  def create_table(table_name, definition, opts \\ []) do
    db_name = opts[:db_name] || table_name
    with {:ok, _} <- create_db_table(db_name, definition, opts), do:
      DataSource.register_test_table(String.to_atom(table_name), full_table_name(db_name), "user_id")
  end

  def add_users_data(data) do
    for {user_id, user_data} <- data, {table_name, table_data} <- user_data do
      insert_rows(user_id, table_name, table_data)
    end
    :ok
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

  defp insert_rows(user_id, table_name, table_data) do
    columns = Enum.map(["user_id" | Keyword.fetch!(table_data, :columns)], &sanitize_db_object/1)
    rows = Enum.map(Keyword.fetch!(table_data, :data), fn(row) -> [user_id | row] end)
    placeholders = 1..length(columns)
    |> Enum.map(fn(index) -> "$#{index}" end)
    |> Enum.join(",")

    query = """
    INSERT INTO #{sanitized_table(table_name)}
    (#{Enum.join(columns, ",")})
    VALUES(#{placeholders})
    """

    for row <- rows, do: {:ok, _} = PostgreSQL.execute(query, row)
    :ok
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
