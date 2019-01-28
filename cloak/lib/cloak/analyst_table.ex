defmodule Cloak.AnalystTable do
  @doc "Service for working with analyst tables"

  use GenServer
  alias Cloak.AnalystTable.Helpers
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores the analyst table to the database."
  @spec store(Cloak.Sql.Query.analyst_id(), String.t(), String.t(), DataSource.t()) :: :ok | {:error, String.t()}
  def store(analyst, table_name, statement, data_source) do
    GenServer.call(
      __MODULE__,
      {:store, %{analyst: analyst, name: table_name, statement: statement, data_source: data_source}},
      :timer.hours(1)
    )
  end

  @doc "Returns the analyst table definition."
  @spec table_definition(Cloak.Sql.Query.analyst_id(), String.t(), DataSource.t()) ::
          {:ok, DataSource.Table.t()} | {:error, String.t()}
  def table_definition(analyst, table_name, data_source),
    do: GenServer.call(__MODULE__, {:table_definition, analyst, table_name, data_source}, :timer.hours(1))

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil), do: {:ok, %{tables: %{}}}

  @impl GenServer
  def handle_call({:store, table}, _from, state) do
    case store_table_to_database(table) do
      {:ok, db_name} -> {:reply, :ok, store_table_to_state(state, Map.put(table, :db_name, db_name))}
      {:error, _reason} = error -> {:reply, error, state}
    end
  end

  def handle_call({:table_definition, analyst, table_name, data_source}, _from, state) do
    response =
      with {:ok, table_data} <- table_data(state, analyst, table_name, data_source),
           {:ok, query} <- Helpers.compile(table_name, table_data.statement, data_source),
           do: {:ok, table_definition(table_data, query)}

    {:reply, response, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp store_table_to_database(table) do
    with {:ok, query} <- Helpers.compile(table.name, table.statement, table.data_source),
         do: Helpers.store({table.analyst, table.name}, query, table.data_source)
  end

  defp store_table_to_state(state, table) do
    update_in(
      state.tables[table.data_source.name],
      &put_table(&1, Map.take(table, [:analyst, :name, :statement, :db_name]))
    )
  end

  defp put_table(nil, table), do: [table]
  defp put_table([], table), do: [table]
  defp put_table([%{analyst: analyst, name: name} | rest], %{analyst: analyst, name: name} = table), do: [table | rest]
  defp put_table([other_table | rest], table), do: [other_table | put_table(rest, table)]

  defp table_data(state, analyst, table_name, data_source) do
    with {:ok, tables} <- Map.fetch(state.tables, data_source.name),
         table_data when not is_nil(table_data) <- Enum.find(tables, &(&1.analyst == analyst && &1.name == table_name)),
         do: {:ok, table_data},
         else: (_ -> {:error, "Table not found."})
  end

  defp table_definition(table_data, query) do
    DataSource.Table.new(
      table_data.name,
      Cloak.Sql.Compiler.Helpers.id_column(query).name,
      %{db_name: table_data.db_name, columns: columns(query)}
    )
  end

  defp columns(query) do
    Enum.map(
      Enum.zip(query.column_titles, query.columns),
      fn {column_title, column} -> DataSource.Table.column(column_title, column.type) end
    )
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
