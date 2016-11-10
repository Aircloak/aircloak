defmodule Air.Service.AuditLog do
  @moduledoc "Services for using the audit log."

  alias Air.{Repo, AuditLog, DataSource, User}
  import Ecto.Query, only: [from: 2]
  require Logger

  @type filter_params :: %{
    page: non_neg_integer,
    users: [String.t],
    events: [String.t],
    data_sources: [String.t]
  }

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
  @spec for(filter_params) :: Scrivener.Page.t
  def for(params) do
    AuditLog
    |> for_user(params.users)
    |> for_event(params.events)
    |> for_data_sources(params.data_sources)
    |> order_by_event()
    |> Repo.paginate(page: params.page)
  end

  @doc """
  Returns a list of distinct event types given a set of users.
  If no users are given, all event types across all users are returned.
  """
  @spec event_types(filter_params) :: [String.t]
  def event_types(params) do
    AuditLog
    |> for_user(params.users)
    |> for_data_sources(params.data_sources)
    |> select_event_types()
    |> Repo.all()
  end

  @doc """
  Returns a list of the distinct data sources having been queried.
  Returns an empty list if none of the audit log entries currently
  being filtered for are query execution events.
  """
  @spec data_sources(filter_params) :: [%{id: non_neg_integer, name: String.t}]
  def data_sources(params) do
    AuditLog
    |> for_user(params.users)
    |> for_event(params.events)
    |> select_data_sources()
    |> Repo.all()
  end

  @doc """
  Returns user structs (names and emails) of users who have audit log
  events for a given filter group.
  """
  @spec users(filter_params) :: [%{name: String.t, email: String.t}]
  def users(params) do
    AuditLog
    |> for_event(params.events)
    |> for_data_sources(params.data_sources)
    |> select_users()
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

  defp for_data_sources(query, []), do: query
  defp for_data_sources(query, data_sources) do
    data_sources = data_sources |> Enum.map(&to_string/1)
    from a in query,
    where: fragment("?->>'data_source'", a.metadata) in ^data_sources
  end

  defp select_event_types(query) do
    from a in query,
    group_by: a.event,
    order_by: [asc: :event],
    select: a.event
  end

  defp select_data_sources(query) do
    data_source_query = from data_source in DataSource,
      select: %{
        id: data_source.id,
        name: data_source.name,
      }

    from a in query,
    where: fragment("?->>'data_source' <> ''", a.metadata),
    right_join: d in subquery(data_source_query),
    on: d.id == fragment("cast(?->>'data_source' as integer)", a.metadata),
    group_by: [d.id, d.name],
    order_by: [asc: d.name],
    select: %{
      id: d.id,
      name: d.name,
    }
  end

  defp select_users(query) do
    user_query = from user in User,
      select: %{
        name: user.name,
        email: user.email,
      }

    from a in query,
    inner_join: user in subquery(user_query),
    on: a.user == user.email,
    group_by: [user.name, user.email],
    order_by: [asc: user.name],
    select: user
  end
end
