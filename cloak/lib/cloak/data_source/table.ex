defmodule Cloak.DataSource.Table do
  @moduledoc "Provides functionality for working with tables."

  alias Cloak.DataSource
  alias Cloak.Query.{DataDecoder, ExecutionError}

  require Logger

  @type data_type :: :text | :integer | :real | :boolean | :datetime | :time | :date | :uuid | :unknown
  @type column :: %{name: String.t, type: data_type, visible?: boolean}

  @type t :: %{
    :name => String.t, # table name as seen by the user
    :db_name => String.t, # table name in the database
    :user_id => String.t,
    :ignore_unsupported_types => boolean,
    :columns => [column],
    :decoders => [DataDecoder.t],
    :projection => %{table: String.t, primary_key: String.t, foreign_key: String.t} | nil,
    optional(any) => any,
  }


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates the column entry in the table specification."
  @spec column(String.t, data_type, [visible?: boolean]) :: column
  def column(name, type, optional_params \\ []), do:
    %{name: name, type: type, visible?: Keyword.get(optional_params, :visible?, true)}

  @doc "Given a data source and a connection to it, it will load all configured tables from the data set. "
  @spec load(DataSource.t, DataSource.Driver.connection) :: DataSource.t
  def load(data_source, connection), do:
    data_source |> scan_tables(connection) |> resolve_projected_tables()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp scan_tables(%{errors: existing_errors} = data_source, connection) do
    {tables, errors} =
      Enum.reduce(data_source.tables, {[], []}, fn (table, {tables, errors}) ->
        try do
          {tables ++ load_tables(data_source, connection, table), errors}
        rescue
          error in ExecutionError ->
            {table_id, _} = table
            message = "Load error for table `#{table_id}`: #{Exception.message(error)}."
            Logger.error("Data source `#{data_source.name}`: #{message}")
            {tables, errors ++ [message]}
        end
      end)
    %{data_source | errors: existing_errors ++ errors, tables: Enum.into(tables, %{})}
  end

  defp load_tables(data_source, connection, {table_id, table}) do
    table_id = to_string(table_id)
    table =
      table
      |> Map.put(:columns, [])
      |> Map.put(:name, table_id)
      |> Map.put_new(:db_name, table_id)
      |> Map.put_new(:decoders, [])
      |> Map.put_new(:user_id, nil)
      |> Map.put_new(:projection, nil)

    data_source.driver.load_tables(connection, table)
    |> Enum.map(&parse_columns(data_source, &1))
    |> Enum.map(&DataDecoder.init/1)
    |> Enum.map(&{String.to_atom(&1.name), &1})
  end

  defp parse_columns(data_source, table) do
    table.columns
    |> Enum.reject(&supported?/1)
    |> validate_unsupported_columns(data_source, table)
    columns = for column <- table.columns, do:
      if supported?(column), do: column, else: %{column | type: :unknown}
    table = %{table | columns: columns}
    verify_columns(table)
    table
  end

  defp verify_columns(table) do
    verify_user_id(table)
    if table.columns == [], do: DataSource.raise_error("no data columns found in table")
  end

  defp verify_user_id(%{projection: projection}) when projection != nil, do: :ok
  defp verify_user_id(table) do
    user_id = table.user_id
    case Enum.find(table.columns, &(&1.name == user_id)) do
      %{} = column ->
        unless column.type in [:integer, :text, :uuid, :real, :unknown], do:
          DataSource.raise_error("unsupported user id type: #{column.type}")
      nil ->
        columns_string =
          table.columns
          |> Enum.map(&"`#{&1.name}`")
          |> Enum.join(", ")
        DataSource.raise_error("the user id column `#{user_id}` for table `#{table.name}` does not exist. " <>
          "Available columns are: #{columns_string}.")
    end
  end

  defp supported?(%{type: {:unsupported, _db_type}}), do: false
  defp supported?(_column), do: true

  defp validate_unsupported_columns([], _data_source, _table), do: nil
  defp validate_unsupported_columns(unsupported, data_source, table) do
    columns_string =
      unsupported
      |> Enum.map(fn(column) -> "`#{column.name}`::#{inspect(column.type)}" end)
      |> Enum.join(", ")

    if table[:ignore_unsupported_types] do
      Logger.warn("The following columns from table `#{table[:db_name]}` in data source `#{data_source.name}` " <>
        "have unsupported types:\n" <> columns_string)
      nil
    else
      DataSource.raise_error("unsupported types for columns: #{columns_string} "
        <> "(to ignore these columns set 'ignore_unsupported_types: true' in your table settings)")
    end
  end

  defp resolve_projected_tables(data_source) do
    # remove the set user_id for projected tables
    tables =
      data_source.tables
      |> Enum.map(fn
        ({id, %{projection: nil} = table}) -> {id, table}
        ({id, table}) -> {id, %{table | user_id: nil}}
      end)
      |> Enum.into(%{})
    data_source = %{data_source | tables: tables}
    tables
    |> Map.keys()
    |> Enum.reduce(data_source, &resolve_projected_table(Map.fetch!(&2.tables, &1), &2))
  end

  defp resolve_projected_table(%{projection: nil}, data_source), do: data_source
  defp resolve_projected_table(%{user_id: uid, columns: [%{name: uid} | _]}, data_source), do:
    data_source # uid column is resolved
  defp resolve_projected_table(table, data_source) do
    case validate_projection(data_source.tables, table) do
      {:ok, referenced_table} ->
        # recursively resolve dependent table (this allows the dependent table to be projected as well)
        data_source = resolve_projected_table(referenced_table, data_source)
        # refetch the table, since it's maybe updated
        referenced_table = Map.fetch!(data_source.tables, String.to_atom(referenced_table.name))

        uid_column_name = referenced_table.user_id
        uid_column = Enum.find(referenced_table.columns, &(&1.name == uid_column_name))
        table =
          table
          |> Map.put(:user_id, uid_column_name)
          |> Map.put(:columns, [uid_column | table.columns])
        tables = Map.put(data_source.tables, String.to_atom(table.name), table)
        %{data_source | tables: tables}

      {:error, reason} ->
        message = "Projection error in table `#{table.name}`: #{reason}."
        Logger.error("Data source `#{data_source.name}`: #{message}")
        tables = Map.delete(data_source.tables, String.to_atom(table.name))
        %{data_source | tables: tables, errors: data_source.errors ++ [message]}
    end
  end

  defp validate_projection(tables_map, table) do
    with :ok <- validate_foreign_key(table),
         {:ok, referenced_table} <- validate_referenced_table(tables_map, table),
         :ok <- validate_primary_key(table, referenced_table),
    do: {:ok, referenced_table}
  end

  defp validate_referenced_table(tables_map, table) do
    case Map.fetch(tables_map, String.to_atom(table.projection.table)) do
      :error -> {:error, "referenced table `#{table.projection.table}` not found."}
      {:ok, referenced_table} -> {:ok, referenced_table}
    end
  end

  defp validate_foreign_key(table) do
    case Enum.find(table.columns, &(&1.name == table.projection.foreign_key)) do
      nil -> {:error, "foreign key column `#{table.projection.foreign_key}` doesn't exist"}
      _ -> :ok
    end
  end

  defp validate_primary_key(table, referenced_table) do
    foreign_key_type = Enum.find(table.columns, &(&1.name == table.projection.foreign_key)).type
    case Enum.find(referenced_table.columns, &(&1.name == table.projection.primary_key)) do
      nil ->
        {
          :error,
          "primary key column `#{table.projection.primary_key}` not found in table `#{referenced_table.db_name}`"
        }
      %{type: primary_key_type} when primary_key_type != foreign_key_type ->
        {
          :error,
          "foreign key type is `#{foreign_key_type}` while primary key type is `#{primary_key_type}`"
        }
      %{type: ^foreign_key_type} -> :ok
    end
  end
end
