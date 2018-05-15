defmodule Central.Service.StatsDB do
  @moduledoc "Service module for storing and querying stats"

  import Supervisor.Spec

  require Logger
  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stored query stats in MongoDB for analytics purposes."
  @spec record_query(Map.t()) :: :ok | :error
  def record_query(params) do
    case Mongo.insert_one(:mongo, "queries", params, pool: DBConnection.Poolboy) do
      {:ok, _} ->
        :ok

      otherwise ->
        Logger.warn("Could not record query stats: #{inspect(otherwise)}")
        :error
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    provided_config =
      Aircloak.DeployConfig.fetch!("stats_db")
      |> Aircloak.atomize_keys()
      |> Map.to_list()

    worker(Mongo, [[name: :mongo, pool: DBConnection.Poolboy] ++ provided_config])
  end
end
