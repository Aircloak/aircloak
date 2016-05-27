defmodule Air.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to queries.
  For the time being no incoming messages are supported,
  but we do support two outgoing type of messages:

  - __result__: reports new results as queries finish executing
  """
  use Air.Web, :channel
  require Logger
  alias Air.{Query, Repo}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Broadcasts the results of a query execution to all listening clients.
  """
  @spec broadcast_result(Query.result) :: :ok
  def broadcast_result(result) do
    query = Repo.get!(Query, result["query_id"])
    payload = Map.put(result, "query", query.query)

    Air.Endpoint.broadcast_from!(self(), "user:#{query.user_id}", "result", payload)

    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("user:" <> user_id, _, socket) do
    if socket.assigns.user.id == String.to_integer(user_id) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Channel not found"}}
    end
  end
end
