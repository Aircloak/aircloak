defmodule Air.QueryLifecycle do
  @moduledoc """
  Serialized processing of query events.

  Since there might be multiple parallel events regarding a single query, such as results or state changes, we
  serialize these changes through this process.
  """

  import Supervisor.Spec, warn: false
  alias Air.{Repo, Schemas.Query, Socket.Frontend.UserChannel}
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a worker specification for the query result processor"
  @spec observer_spec() :: Supervisor.Spec.spec
  def observer_spec do
    worker(Task, [fn() ->
      for {:query_result, result} <- Air.QueryEvents.stream, do: process_result(result)
    end])
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp process_result(result) do
    query = Repo.get!(Query, result["query_id"])

    row_count = (result["rows"] || []) |> Enum.map(&(&1["occurrences"])) |> Enum.sum

    storable_result = %{
      columns: result["columns"],
      types: result["features"]["selected_types"],
      rows: result["rows"],
      error: result["error"],
      info: result["info"],
      row_count: row_count,
    }

    query
    |> Query.changeset(%{
      result: storable_result,
      execution_time: result["execution_time"],
      users_count: result["users_count"],
      features: result["features"],
      query_state: :completed,
    })
    |> Repo.update!()
    |> UserChannel.broadcast_result()

    Logger.info("processed result for query #{result["query_id"]}")
  end
end
