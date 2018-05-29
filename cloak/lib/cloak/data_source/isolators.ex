defmodule Cloak.DataSource.Isolators do
  @moduledoc "Entry point for checking if a column is isolating."

  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(DataSource.t(), String.t(), String.t()) :: boolean
  if Mix.env() == :test do
    def isolates_users?(data_source, table, column) do
      Agent.get(__MODULE__, &MapSet.member?(&1, {data_source[:name], table, column}))
    end
  else
    def isolates_users?(data_source, table, column) do
      __MODULE__.Query.isolates_users?(data_source, table, column)
    end
  end

  if Mix.env() == :test do
    @doc false
    def register_isolating_column(data_source, table, column) do
      Agent.update(__MODULE__, &MapSet.put(&1, {data_source[:name], table, column}))
    end
  end

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_options \\ []) do
    import Aircloak.ChildSpec, only: [supervisor: 2]
    import Aircloak, only: [in_env: 1, unused: 2]

    unused(test_agent_spec(), in: [:dev, :prod])

    supervisor(
      [
        in_env(test: test_agent_spec(), else: nil),
        Cloak.DataSource.Isolators.Cache
      ]
      |> Enum.reject(&is_nil/1),
      strategy: :one_for_one
    )
  end

  defp test_agent_spec(), do: Aircloak.ChildSpec.agent(fn -> MapSet.new() end, name: __MODULE__)
end
