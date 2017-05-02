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
        worker(Task, [&handle_query_results/0], id: Module.concat(__MODULE__, Worker))
      ],
      strategy: :one_for_one
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp handle_query_results() do
    for {:query_result, result} <- Air.QueryEvents.stream() do
      Task.Supervisor.start_child(@task_supervisor, fn() -> process_result(result) end)
    end
  end

  defp process_result(result) do
    query = Repo.get!(Query, result["query_id"]) |> Repo.preload([:user, :data_source])
    user = query.user || %{
      name: "Unknown user",
      email: "Unknown email",
    }
    data_source = query.data_source || %{
      name: "Unknown data source",
      id: nil,
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
          id: data_source.id,
        },
        started_at: query.inserted_at,
        finished_at: NaiveDateTime.utc_now(),
        has_error: not is_nil(result["error"]),
        error: filter_error(result["error"]),
      },
    }
    Air.Service.Central.record_query(payload)
  end

  defp filter_error(nil), do: nil
  defp filter_error(error), do: Air.Service.Redacter.filter_query_error(error)
end

