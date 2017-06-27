defmodule Air.Service.Query.Lifecycle do
  @moduledoc """
  Serialized processing of query events.

  Since there might be multiple parallel events regarding a single query, such as results or state changes, we
  serialize these changes through this process.
  """

  import Supervisor.Spec, warn: false
  alias Air.Service.Query


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a worker specification for the query result processor"
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec, do: worker(Task, [&run/0])


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run() do
    for event <- Air.Service.Query.Events.stream do
      case event do
        {:query_result, result} -> Query.process_result(result)
        {:query_state_change, query_id, state} -> Query.update_state(query_id, state)
        {:query_died, query_id} -> Query.query_died(query_id)
        _ -> :ignore
      end
    end
  end
end
