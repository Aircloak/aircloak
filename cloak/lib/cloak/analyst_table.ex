defmodule Cloak.AnalystTable do
  @doc "Service for working with analyst tables"

  use GenServer
  alias Cloak.AnalystTable.Helpers

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores the analyst table to the database."
  @spec store(any, String.t(), Cloak.DataSource.t()) :: {:ok, table_name :: String.t()} | {:error, String.t()}
  def store(id, statement, data_source),
    do: GenServer.call(__MODULE__, {:store, id, statement, data_source}, :timer.hours(1))

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil), do: {:ok, nil}

  @impl GenServer
  def handle_call({:store, id, statement, data_source}, _from, state) do
    response = with {:ok, query} <- Helpers.compile(statement, data_source), do: Helpers.store(id, query, data_source)
    {:reply, response, state}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
