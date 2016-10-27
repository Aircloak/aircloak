defmodule Air.ResultProcessor do
  @moduledoc """
  Concurrent processing of query results.

  Processing of a query result requires multiple actions, such as interpreting
  data, storing the information into the database, and notifying interested
  parties. Since many things can go wrong here, we're doing the job concurrently
  to limit the effect of a possible failure. If processing of a single result
  fails, nothing else is taken down.
  """

  import Supervisor.Spec, warn: false
  alias Air.{Repo, Query, Socket.Frontend.UserChannel}
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a supervisor specification for the supervisor of result processors.

  All result processors will be running under this supervisor as temporary workers. If
  a processor crashes, an error will be logged, but there won't be any attempts
  to retry the job.
  """
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    supervisor(Task.Supervisor, [[name: __MODULE__, restart: :temporary]], [id: :result_processor])
  end

  def observer_spec do
    worker(Task, [fn() ->
      for {:result, result} <- Air.QueryEvents.stream, do: start_processor(result)
    end])
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
    query = Repo.get!(Query, result["query_id"])

    row_count = (result["rows"] || []) |> Enum.map(&(&1["occurrences"])) |> Enum.sum

    storable_result = Poison.encode!(%{
      columns: result["columns"],
      types: result["features"]["selected_types"],
      rows: result["rows"],
      error: result["error"],
      info: result["info"],
      row_count: row_count,
    })

    query
    |> Query.changeset(%{
      result: storable_result,
      execution_time: result["execution_time"],
      users_count: result["users_count"],
      features: result["features"],
    })
    |> Repo.update!()
    |> UserChannel.broadcast_result()

    Logger.info("processed result for query #{result["query_id"]}")
  end
end
