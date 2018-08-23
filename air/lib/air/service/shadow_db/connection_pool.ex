defmodule Air.Service.ShadowDb.ConnectionPool do
  @moduledoc "Manages a pool of connections to the single shadow database."

  alias Air.Service.ShadowDb.ConnectionOwner

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the given SQL query."
  @spec query(String.t(), String.t(), [term]) :: Air.Service.ShadowDb.Connection.query_result()
  def query(data_source_name, query, params),
    do: :poolboy.transaction(name(data_source_name), &ConnectionOwner.query(&1, query, params))

  @doc "Parses the given SQL query."
  @spec parse(String.t(), String.t()) :: Air.Service.ShadowDb.Connection.parse_result()
  def parse(data_source_name, query),
    do: :poolboy.transaction(name(data_source_name), &ConnectionOwner.parse(&1, query))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(data_source_name), do: Air.Service.ShadowDb.registered_name(data_source_name, __MODULE__)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(data_source_name) do
    :poolboy.child_spec(
      {__MODULE__, data_source_name},
      [
        name: name(data_source_name),
        worker_module: ConnectionOwner,
        max_overflow: 0,
        size: 10
      ],
      [data_source_name]
    )
  end
end
