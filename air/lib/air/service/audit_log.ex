defmodule Air.Service.AuditLog do
  @moduledoc "Services for using the audit log."

  alias Air.{Repo, Schemas.AuditLog, Schemas.DataSource, Schemas.User}
  import Ecto.Query, only: [limit: 2, from: 2, subquery: 1, offset: 2, preload: 2]
  require Logger

  @type login :: String.t()
  @type event_name :: String.t()
  @type data_source_id :: non_neg_integer
  @type user_id :: non_neg_integer
  @type filter_params :: %{
          from: DateTime.t(),
          to: DateTime.t(),
          users: [user_id],
          events: [event_name],
          data_sources: [data_source_id],
          except: [non_neg_integer],
          max_results: non_neg_integer,
          page: non_neg_integer
        }

  @type group :: %{
          id: non_neg_integer,
          user_id: non_neg_integer,
          user_name: String.t(),
          event: event_name,
          metadata: any,
          occurences: non_neg_integer,
          min_date: DateTime.t(),
          max_date: DateTime.t()
        }

  @type paginated_list(item) :: {boolean, [item]}

  @type by_date :: {DateTime.t(), [group]}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates an audit log entry."
  @spec log(User.t(), String.t(), %{atom => any}) :: :ok | {:error, any}
  def log(user, event, metadata \\ %{}) do
    if Air.Service.Settings.read().audit_log_enabled do
      user
      |> Ecto.build_assoc(:audit_logs)
      |> AuditLog.changeset(%{event: event, metadata: compute_diff(metadata)})
      |> Repo.insert()
      |> case do
        {:ok, _} ->
          :ok

        {:error, reason} ->
          Logger.error("Failed at storing audit log entry: #{inspect(reason)}")
          {:error, reason}
      end
    else
      :ok
    end
  end

  @doc """
  Returns audit log entries created in a given time interval (inclusive).

  Returned entries are descending sorted by the creation date.

  Results are paginated. This means the method returns a tuple: `{has_more_pages :: boolean, the_actual_data}`.
  """
  @spec for(filter_params) :: paginated_list(AuditLog.t())
  def for(params) do
    AuditLog
    |> for_time(params.from, params.to)
    |> for_user(params.users)
    |> for_event(params.events)
    |> for_data_sources(params.data_sources)
    |> for_exceptions(params.except)
    |> order_by_event()
    |> preload(user: :logins)
    |> fetch_paginated(params)
  end

  @doc """
  Returns audit log entries in a format designed for display:

  - The list is grouped by days in order to show headers
  - Each item is basically the first event in a group (sorted by time in descending order)
  - Some extra attributes are added
  - A group is events with the same user and event_type occuring on the same day, consecutively.

  For example:

       [{"Created", "Johny", 2020-10-01 10:20:00},
        {"Created", "Johny", 2020-10-01 10:30},
        {"Snoozed", "Benny", 2020-10-01 10:40},
        {"Created", "Johny", 2020-10-01 10:50},
        {"Created", "Johny", 2020-10-12 11:00}]

  Would get returned as:

       [{"Created", "Johny", 2, 2020-10-01 10:20:00, 2020-10-01 10:30},
        {"Snoozed", "Benny", 1, 2020-10-01 10:40, 2020-10-01 10:40}, # This is a different event and user
        {"Created", "Johny", 1, 2020-10-01 10:50, 2020-10-01 10:50},
        {"Created", "Johny", 1, 2020-10-12 11:00, 2020-10-12 11:00}] # This is a different day

  Results are paginated. This means the method returns a tuple: `{has_more_pages :: boolean, the_actual_data}`.
  """
  @spec grouped_for(filter_params) :: paginated_list(by_date)
  def grouped_for(params) do
    AuditLog
    |> for_time(params.from, params.to)
    |> for_user(params.users)
    |> for_event(params.events)
    |> for_data_sources(params.data_sources)
    |> for_exceptions(params.except)
    |> order_by_event()
    |> grouped()
    |> fetch_paginated(params)
    |> update_in([Access.elem(1)], &group_by_day/1)
  end

  @doc "Used for dealing with pagination. Similar to `++`, but makes sure that the property of being grouped by day is preserved."
  @spec merged_grouped_lists([by_date], [by_date]) :: [by_date]
  def merged_grouped_lists(as, [{date, list} | bs]) do
    last = Access.at(length(as) - 1)

    if Timex.beginning_of_day(get_in(as, [last, Access.elem(0)])) == Timex.beginning_of_day(date) do
      update_in(as, [last, Access.elem(1)], fn list1 -> list1 ++ list end) ++ bs
    else
      as ++ [{date, list} | bs]
    end
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
  Returns user structs (names and logins) of users who have audit log
  events for a given filter group.

  Also includes all users present in the parameters, whether or not
  the other parameters would normally exclude them.
  """
  @spec users(filter_params) :: [User.t()]
  def users(params) do
    users =
      AuditLog
      |> for_time(params.from, params.to)
      |> for_event(params.events)
      |> for_data_sources(params.data_sources)
      |> select_users()
      |> Repo.all()

    selected_users = selected_users(params)

    (users ++ selected_users)
    |> Enum.uniq_by(& &1.id)
    |> Enum.sort_by(& &1.name)
  end

  @doc "Returns the number of audit log entries"
  @spec count() :: integer
  def count(), do: Repo.one(from(audit_log_entry in AuditLog, select: count(audit_log_entry.id)))

  @doc "Returns login events"
  def login_events() do
    Repo.all(
      from a in AuditLog,
      where: fragment("? > now() - interval '1 hour'", a.inserted_at),
      where: fragment("lower(?)", a.event) in ["failed login", "logged in"],
      order_by: [desc: a.inserted_at],
      preload: [:user],
      select: a
    )
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp order_by_event(query) do
    from(a in query, order_by: [desc: :inserted_at])
  end

  defp compute_diff(metadata = %{before: prev, after: next}) do
    rest = metadata |> Map.delete(:before) |> Map.delete(:after)

    new = Map.delete(next, :__struct__)

    difference =
      Enum.reduce(Map.delete(prev, :__struct__), %{}, fn {key, val}, diff ->
        if val == new[key] || key in [:updated_at] do
          diff
        else
          Map.put(diff, key |> Atom.to_string() |> String.replace("_", " ") |> String.capitalize(), %{
            before: to_description(val),
            after: to_description(new[key])
          })
        end
      end)

    if Enum.empty?(difference), do: rest, else: Map.put(rest, :diff, difference)
  end

  defp compute_diff(metadata), do: metadata

  defp to_description(v) when is_list(v), do: Enum.join(Enum.map(v, &to_description/1), ", ")
  defp to_description(%{:name => name}), do: name
  defp to_description(v), do: v

  defp fetch_paginated(query, %{max_results: max_results, page: page}) do
    results =
      query
      |> limit(^(max_results + 1))
      |> offset(^((page - 1) * max_results))
      |> Repo.all()

    {length(results) > max_results, Enum.take(results, max_results)}
  end

  defmacrop first(col) do
    quote do
      fragment("(array_agg(?))[1]", unquote(col))
    end
  end

  defp grouped(query) do
    from(
      b in subquery(
        from(a in query,
          select: %{
            id: a.id,
            user_id: a.user_id,
            event: a.event,
            inserted_at: a.inserted_at,
            metadata: a.metadata,
            grouping_id:
              over(row_number(), order_by: [desc: :inserted_at]) -
                over(row_number(),
                  partition_by: [a.user_id, a.event, fragment("date_trunc(?, ?)", "day", a.inserted_at)],
                  order_by: [desc: :inserted_at]
                )
          },
          order_by: [desc: :inserted_at]
        )
      ),
      join: user in User,
      on: b.user_id == user.id,
      group_by: [b.grouping_id, b.user_id, b.event, user.name],
      select: %{
        id: first(b.id),
        user_id: b.user_id,
        user_name: user.name,
        event: b.event,
        metadata: first(b.metadata),
        occurences: count(b.grouping_id),
        min_date: min(b.inserted_at),
        max_date: max(b.inserted_at)
      },
      order_by: [desc: max(b.inserted_at)]
    )
  end

  @spec group_by_day([group]) :: [by_date]
  defp group_by_day(data) do
    data
    |> Enum.reduce(
      [],
      fn element, agg ->
        case agg do
          [{date, list} | rest] ->
            if Timex.beginning_of_day(element.max_date) == Timex.beginning_of_day(date) do
              [{date, [element | list]} | rest]
            else
              [{element.max_date, [element]} | [{date, Enum.reverse(list)} | rest]]
            end

          _ ->
            [{element.max_date, [element]}]
        end
      end
    )
    |> update_in([Access.at(0), Access.elem(1)], &Enum.reverse/1)
    |> Enum.reverse()
  end

  defp for_user(query, []), do: query

  defp for_user(query, users) do
    from(a in query, where: a.user_id in ^users)
  end

  defp for_event(query, []), do: query

  defp for_event(query, events) do
    from(a in query, where: a.event in ^events)
  end

  defp for_exceptions(query, []), do: query

  defp for_exceptions(query, exceptions) do
    from(a in query, where: a.id not in ^exceptions)
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
    from(
      user in User,
      join: log in ^query,
      on: user.id == log.user_id,
      distinct: user.id,
      select: user,
      preload: :logins
    )
  end

  defp selected_users(%{users: users}) do
    from(
      user in User,
      where: user.id in ^users,
      select: user,
      preload: :logins
    )
    |> Repo.all()
  end
end
