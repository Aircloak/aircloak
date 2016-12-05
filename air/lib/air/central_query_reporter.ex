defmodule Air.CentralQueryReporter do
  @moduledoc """
  Reports query executions to the Airport Central
  for book keeping and stats.

  Currently fails silently when a query execution report
  cannot be delivered successfully.
  """

  import Supervisor.Spec, warn: false
  require Logger
  alias Air.{Repo, Schemas.Query}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a supervisor specification for the supervisor of query reporter.

  All query reporters will be running under this supervisor as temporary workers. If
  a processor crashes, an error will be logged, but there won't be any attempts
  to retry the job.
  """
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    supervisor(Task.Supervisor, [[name: __MODULE__, restart: :temporary]], [id: :central_query_reporter])
  end

  @doc "Returns a worker specification for the query result processor"
  @spec observer_spec() :: Supervisor.Spec.spec
  def observer_spec do
    worker(Task, [fn() ->
      for {:result, result} <- Air.QueryEvents.stream, do: start_processor(result)
    end], id: :central_query_reporter_worker)
  end

  @doc "Starts a result processor."
  @spec start_processor(%{String.t => any}) :: {:ok, pid}
  def start_processor(result) do
    Task.Supervisor.start_child(__MODULE__, fn() -> process_result(result) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp process_result(result) do
    query = Repo.get!(Query, result["query_id"]) |> Repo.preload([:user, :data_source])
    user = query.user || %{
      name: "Uknown user",
      email: "Uknown email",
    }
    data_source = query.data_source || %{
      name: "Unknown data source",
      global_id: "Unknown data source",
    }

    row_count = (result["rows"] || []) |> Enum.map(&(&1["occurrences"])) |> Enum.sum
    payload = %{
      metrics: %{
        users_count: result["users_count"],
        row_count: row_count,
        execution_time: result["execution_time"],
      },
      features: result["features"],
      aux: %{
        user: %{
          name: user.name,
          email: user.email,
        },
        data_source: %{
          name: data_source.name,
          id: data_source.global_id,
        }
      },
    }
    Air.CentralSocket.record_query(payload)
  end
end

