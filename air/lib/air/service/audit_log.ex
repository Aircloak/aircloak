defmodule Air.Service.AuditLog do
  @moduledoc "Services for using the audit log."

  alias Air.{Repo, AuditLog}
  import Ecto.Query, only: [from: 2]
  require Logger


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Creates an audit log entry."
  @spec log(nil | Air.User.t, String.t, %{atom => any}) :: :ok | {:error, any}
  def log(user, event, metadata \\ %{}) do
    email = if user != nil, do: user.email, else: "Unknown user"

    %AuditLog{}
    |> AuditLog.changeset(%{user: email, event: event, metadata: metadata})
    |> Repo.insert()
    |> case do
          {:ok, _} -> :ok
          {:error, reason} ->
            Logger.error("Failed at storing audit log entry: #{inspect(reason)}")
            {:error, reason}
        end
  end

  @doc """
  Returns audit log entries created in a given time interval (inclusive).

  Returned entries are descending sorted by the creation date.
  """
  @spec for(Map.t) :: Scrivener.Page.t
  def for(params \\ %{}) do
    AuditLog
    |> for_user(Map.get(params, :users, []))
    |> for_event(Map.get(params, :events, []))
    |> order_by_event()
    |> Repo.paginate(page: Map.get(params, :page, 1))
  end

  @doc """
  Returns a list of distinct event types given a set of users.
  If no users are given, all event types across all users are returned.
  """
  @spec event_types([String.t]) :: [String.t]
  def event_types(users \\ []) do
    AuditLog
    |> for_user(users)
    |> select_event_types()
    |> Repo.all()
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp order_by_event(query) do
    from a in query,
    order_by: [desc: :inserted_at]
  end

  defp for_user(query, []), do: query
  defp for_user(query, users) do
    from a in query,
    where: a.user in ^users
  end

  defp for_event(query, []), do: query
  defp for_event(query, events) do
    from a in query,
    where: a.event in ^events
  end

  defp select_event_types(query) do
    from a in query,
    group_by: a.event,
    order_by: [asc: :event],
    select: a.event
  end
end
