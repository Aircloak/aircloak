defmodule Air.CentralClient.QueryReporter do
  @moduledoc """
  Reports query executions to the Airport Central
  for book keeping and stats.

  Currently fails silently when a query execution report
  cannot be delivered successfully.
  """

  require Logger
  alias Air.{Repo, Schemas.Query}

  @task_supervisor Module.concat(__MODULE__, TaskSup)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the reporter supervision tree.

  All query reporters will be running under a dedicated Task supervisor as temporary workers. If
  a processor crashes, an error will be logged, but there won't be any attempts
  to retry the job.
  """
  @spec start_link() :: Supervisor.on_start
  def start_link() do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [
        supervisor(Task.Supervisor, [[name: @task_supervisor, restart: :temporary]], [id: @task_supervisor]),
        worker(Task, [&handle_query_events/0], id: Module.concat(__MODULE__, Worker))
      ],
      strategy: :one_for_one
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp handle_query_events(), do:
    Enum.map(Air.QueryEvents.stream(), fn({:result, result}) ->
      Task.Supervisor.start_child(@task_supervisor, fn() -> process_result(result) end)
    end)

  defp process_result(result) do
    query = Repo.get!(Query, result["query_id"]) |> Repo.preload([:user, :data_source])
    user = query.user || %{
      name: "Unknown user",
      email: "Unknown email",
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
        },
        started_at: query.inserted_at,
        finished_at: NaiveDateTime.utc_now(),
      },
      error: only_whitelisted_fields(result["error"]),
    }
    Air.CentralClient.Socket.record_query(payload)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # The :human_description field contains potentially sensitive data that we shouldn't
  # see, so it is removed. In this case it's sensitive in the sense of potential business
  # intelligence/IP since it has the potential to reveal the design of the customer database.
  defp only_whitelisted_fields(nil), do: nil
  defp only_whitelisted_fields(error), do: Map.take(error, ["type", "location", "context"])
end

