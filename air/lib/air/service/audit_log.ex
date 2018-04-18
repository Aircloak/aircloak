defmodule Air.Service.AuditLog do
  @moduledoc "Services for using the audit log."

  alias Air.{Repo, Schemas.AuditLog, Schemas.DataSource, Schemas.User}
  import Ecto.Query, only: [limit: 2, from: 2]
  require Logger

  @type email :: String.t()
  @type event_name :: String.t()
  @type data_source_id :: non_neg_integer
  @type filter_params :: %{
          from: DateTime.t(),
          to: DateTime.t(),
          users: [email],
          events: [event_name],
          data_sources: [data_source_id],
          max_results: non_neg_integer
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates an audit log entry."
  @spec log(nil | User.t(), String.t(), %{atom => any}) :: :ok | {:error, any}
  def log(user, event, metadata \\ %{}) do
    if Air.Service.Settings.read().audit_log_enabled do
      user
      |> Ecto.build_assoc(:audit_logs)
      |> AuditLog.changeset(%{event: event, metadata: metadata})
      |> Repo.insert()
      |> case do
        {:ok, _} ->
          :ok

        {:error, reason} ->
          Logger.error("Failed at storing audit log entry: #{inspect(reason)}")
          {:error, reason}
      end
    else
      # Logging is disabled, so audit logging becomes a noop
    end
  end

  @doc """
  Returns audit log entries created in a given time interval (inclusive).

  Returned entries are descending sorted by the creation date.
  """
  @spec for(filter_params) :: [AuditLog.t()]
  def for(params) do
    AuditLog
    |> for_time(params.from, params.to)
    |> for_user(params.users)
    |> for_event(params.events)
    |> for_data_sources(params.data_sources)
    |> order_by_event()
    |> limit(^params.max_results)
    |> Repo.all()
  end

  @doc """
  Returns a list of distinct event types given a set of users.
  If no users are given, all event types across all users are returned.

  Also includes all events present in the parameters, whether or not
  the other parameters would normally exclude them.
  """
  @spec event_types(filter_params) :: [event_name]
  def event_types(params) do
    event_types =
      AuditLog
      |> for_time(params.from, params.to)
      |> for_user(params.users)
      |> for_data_sources(params.data_sources)
      |> select_event_types()
      |> Repo.all()

    # Include currently selected event types
    (params[:events] ++ event_types)
    |> Enum.uniq()
    |> Enum.sort()
  end

  @doc """
  Returns a list of the distinct data sources having been queried.
  Returns an empty list if none of the audit log entries currently
  being filtered for are query execution events.

  Also includes all data sources present in the parameters, whether or not
  the other parameters would normally exclude them.
  """
  @spec data_sources(filter_params) :: [%{id: data_source_id, name: String.t()}]
  def data_sources(params) do
    data_sources =
      AuditLog
      |> for_time(params.from, params.to)
      |> for_user(params.users)
      |> for_event(params.events)
      |> select_data_sources()
      |> Repo.all()

    # Include currently selected data sources
    (params[:data_sources] -- (data_sources |> Enum.map(& &1.name)))
    |> Air.Service.DataSource.by_names()
    |> Enum.map(&%{name: &1.name})
    |> Enum.concat(data_sources)
    |> Enum.sort_by(& &1.name)
  end

  @doc """
  Returns user structs (names and emails) of users who have audit log
  events for a given filter group.

  Also includes all users present in the parameters, whether or not
  the other parameters would normally exclude them.
  """
  @spec users(filter_params) :: [%{name: String.t(), email: email}]
  def users(params) do
    users =
      AuditLog
      |> for_time(params.from, params.to)
      |> for_event(params.events)
      |> for_data_sources(params.data_sources)
      |> select_users()
      |> Repo.all()

    # Include currently selected users
    (params[:users] -- (users |> Enum.map(& &1.email)))
    |> Air.Service.User.by_emails()
    |> Enum.map(&%{name: &1.name, email: &1.email})
    |> Enum.concat(users)
    |> Enum.sort_by(& &1.name)
  end

  @doc "Returns the number of audit log entries"
  @spec count() :: integer
  def count(), do: Repo.one(from(audit_log_entry in AuditLog, select: count(audit_log_entry.id)))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp order_by_event(query) do
    from(a in query, order_by: [desc: :inserted_at])
  end

  defp for_user(query, []), do: query

  defp for_user(query, users) do
    from(a in query, where: a.user in ^users)
  end

  defp for_event(query, []), do: query

  defp for_event(query, events) do
    from(a in query, where: a.event in ^events)
  end

  defp for_data_sources(query, []), do: query

  defp for_data_sources(query, data_sources) do
    data_sources = data_sources |> Enum.map(&to_string/1)
    from(a in query, where: fragment("?->>'data_source'", a.metadata) in ^data_sources)
  end

  defp for_time(query, from, to) do
    from(a in query, where: a.inserted_at >= ^from and a.inserted_at <= ^to)
  end

  defp select_event_types(query) do
    from(
      a in query,
      group_by: a.event,
      order_by: [asc: :event],
      select: a.event
    )
  end

  defp select_data_sources(query) do
    data_source_query =
      from(
        data_source in DataSource,
        select: %{
          id: data_source.id,
          name: data_source.name
        }
      )

    from(
      audit_log in query,
      where: fragment("?->>'data_source' <> ''", audit_log.metadata),
      right_join: data_source in subquery(data_source_query),
      on: data_source.name == fragment("?->>'data_source'", audit_log.metadata),
      group_by: [data_source.id, data_source.name],
      order_by: [asc: data_source.name],
      select: %{
        id: data_source.id,
        name: data_source.name
      }
    )
  end

  defp select_users(query) do
    user_query =
      from(
        user in User,
        select: %{
          name: user.name,
          email: user.email
        }
      )

    from(
      a in query,
      inner_join: user in subquery(user_query),
      on: a.user == user.email,
      group_by: [user.name, user.email],
      order_by: [asc: user.name],
      select: user
    )
  end
end
