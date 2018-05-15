defmodule Cloak.SapIqHelper do
  @moduledoc "Helper functions for working with SAP IQ database."

  @type conn :: :odbc.connection_reference()
  @type connection_params :: %{
          hostname: String.t(),
          port: pos_integer,
          username: String.t(),
          password: String.t(),
          database: String.t()
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Connects to the database."
  @spec connect(connection_params) :: {:ok, conn} | {:error, any}
  def connect(connection_params) do
    %{
      Host: "#{connection_params.hostname}:#{connection_params.port}",
      UserID: connection_params.username,
      Password: connection_params.password,
      DatabaseName: connection_params.database,
      DSN: "SAPIQ"
    }
    |> Enum.reject(&match?({_key, nil}, &1))
    |> Enum.map(fn {key, value} -> [to_string(key), ?=, value] end)
    |> Enum.join(";")
    |> to_charlist()
    |> :odbc.connect(auto_commit: :on, binary_strings: :on, tuple_row: :off)
  end

  @doc "Executes the database query."
  @spec execute(conn, String.t(), [any]) :: :odbc.result_tuple()
  def execute(conn, command, params \\ []), do: :odbc.param_query(conn, to_charlist(command), params)

  @doc "Executes the database query. Raises on error."
  @spec execute!(conn, String.t(), [any]) :: :ok
  def execute!(conn, command, params \\ []) do
    conn
    |> execute(command, params)
    |> case do
      {:updated, _} ->
        :ok

      {:error, error} ->
        raise to_string(error)
    end
  end

  @doc "Recreates the table according to the provided definition."
  @spec recreate_table!(conn, String.t(), String.t()) :: :ok
  def recreate_table!(conn, table_name, table_def) do
    execute!(conn, ~s/DROP TABLE IF EXISTS "#{table_name}"/)
    execute!(conn, ~s/CREATE TABLE "#{table_name}" (#{table_def})/)
    :ok
  end

  @doc "Inserts multiple rows into the database table."
  @spec insert_rows!(conn, String.t(), [String.t()], [[any]]) :: :ok
  def insert_rows!(conn, table_name, columns, rows) do
    quoted_column_names = columns |> Enum.map(&~s/"#{&1}"/) |> Enum.join(", ")
    value_placeholders = List.duplicate("?", length(columns)) |> Enum.join(", ")
    row_placeholder = "SELECT #{value_placeholders} FROM dummy"

    rows
    |> Stream.chunk(100, 100, [])
    |> Stream.map(fn chunk ->
      select_sql = List.duplicate(row_placeholder, length(chunk)) |> Enum.join(" UNION ALL ")
      values = chunk |> List.flatten() |> Enum.map(&cast_types/1)
      {select_sql, values}
    end)
    |> Enum.each(fn {sql, values} ->
      execute!(
        conn,
        ~s/INSERT INTO "#{table_name}"(#{quoted_column_names}) (#{sql})/,
        values
      )
    end)

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp cast_types(binary) when is_binary(binary) do
    binary = :unicode.characters_to_binary(binary, :utf8, {:utf16, :little})
    {{:sql_wvarchar, length_null_terminated(binary)}, [binary]}
  end

  defp cast_types(integer) when is_integer(integer), do: {:sql_integer, [integer]}
  defp cast_types(float) when is_float(float), do: {:sql_real, [float]}
  defp cast_types(boolean) when is_boolean(boolean), do: {:sql_bit, [boolean]}

  defp cast_types(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_types()

  defp cast_types(nil), do: {:sql_bit, [:null]}

  defp length_null_terminated(binary), do: byte_size(binary) + 1
end
