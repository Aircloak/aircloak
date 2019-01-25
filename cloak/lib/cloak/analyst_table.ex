defmodule Cloak.AnalystTable do
  @doc "Service for working with analyst tables"

  use GenServer
  alias Cloak.AnalystTable.Helpers

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores the analyst table to the database."
  @spec store(any, String.t(), String.t(), Cloak.DataSource.t()) ::
          {:ok, table_name :: String.t()} | {:error, String.t()}
  def store(analyst, table_name, statement, data_source) do
    GenServer.call(
      __MODULE__,
      {:store, %{analyst: analyst, name: table_name, statement: statement, data_source: data_source}},
      :timer.hours(1)
    )
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil), do: {:ok, %{tables: %{}}}

  @impl GenServer
  def handle_call({:store, table}, _from, state) do
    case store_table_to_database(table) do
      {:ok, db_table} -> {:reply, {:ok, db_table}, store_table_to_state(state, Map.put(table, :db_table, db_table))}
      {:error, _reason} = error -> {:reply, error, state}
    end
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
      &put_table(&1, Map.take(table, [:analyst, :name, :statement, :db_table]))
    )
  end

  defp put_table(nil, table), do: [table]
  defp put_table([], table), do: [table]
  defp put_table([%{analyst: analyst, name: name} | rest], %{analyst: analyst, name: name} = table), do: [table | rest]
  defp put_table([other_table | rest], table), do: [other_table | put_table(rest, table)]

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
