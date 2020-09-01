defmodule AirWeb.Admin.AuditLogLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Service.AuditLog

  @impl true
  def mount(_params, session, socket) do
    {
      :ok,
      socket
      |> assign(:expanded, %{})
      |> assign_new(:current_user, fn -> current_user!(session) end)
    }
  end

  @impl true
  def handle_params(params, _uri, socket) do
    from = parse_datetime(params["from"], Timex.now() |> Timex.shift(days: -20))
    to = parse_datetime(params["to"], Timex.now())

    max_results = 1000

    filters = %{
      from: from,
      to: to,
      users: params["users"] || [],
      events: params["events"] || [],
      data_sources: params["data_sources"] || [],
      max_results: max_results
    }

    audit_logs = IO.inspect(AuditLog.grouped_for(filters), label: "Foo")

    {
      :noreply,
      socket
      |> assign(:audit_logs, audit_logs)
      |> assign(:users, AuditLog.users(filters) |> Enum.map(&%{label: &1.name, value: &1.id}))
      |> assign(:event_types, AuditLog.event_types(filters) |> Enum.map(&%{label: &1, value: &1}))
      |> assign(:data_sources, AuditLog.data_sources(filters) |> Enum.map(&%{label: &1.name, value: &1.name}))
      |> assign(:from, from)
      |> assign(:to, to)
      |> assign(:query_params, params)
    }
  end

  @impl true
  def handle_event("change_dates", params, socket) do
    {:noreply,
     push_patch(socket,
       to:
         admin_audit_log_index_path(
           socket,
           :index,
           Map.merge(
             socket.assigns.query_params,
             %{from: params["from"], to: params["to"]}
           )
         )
     )}
  end

  def handle_event("expand_section", params, socket) do
    from = Timex.parse!(params["from"], "{ISOdate} {ISOtime}")
    to = Timex.parse!(params["to"], "{ISOdate} {ISOtime}")
    {user, _} = Integer.parse(params["user"])

    data_sources = socket.assigns.query_params["data_sources"]

    filters = %{
      from: from,
      to: to,
      users: [user],
      events: [params["event"]],
      data_sources: data_sources || [],
      max_results: 1000
    }

    audit_logs = AuditLog.for(filters)

    new_expanded = Map.put(socket.assigns.expanded, {params["event"], user, to}, audit_logs)

    {:noreply,
     socket
     |> assign(:expanded, new_expanded)}
  end

  def handle_event("collapse_section", params, socket) do
    to = Timex.parse!(params["to"], "{ISOdate} {ISOtime}")
    {user, _} = Integer.parse(params["user"])

    new_expanded = Map.delete(socket.assigns.expanded, {params["event"], user, to})

    {:noreply,
     socket
     |> assign(:expanded, new_expanded)}
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp parse_datetime(value, default) do
    case Timex.parse(value, "{ISOdate} {ISOtime}") do
      {:ok, result} ->
        result

      error ->
        default
    end
  end

  defp format_interval(min, max) do
    if Timex.compare(min, max, :day) == 0 do
      "between #{Timex.format!(min, "{h24}:{m}:{s}")} and #{Timex.format!(max, "{h24}:{m}:{s} on the {ISOdate}")}"
    else
      "between #{Timex.format!(min, "{ISOdate} {h24}:{m}:{s}")} and #{Timex.format!(max, "{ISOdate} {h24}:{m}:{s}")}"
    end
  end

  defp current_user!(%{"_air_session_token" => token}) do
    {:ok, user_id} = Air.Service.RevokableToken.verify(token, :session)
    {:ok, user} = Air.Service.User.load_enabled(user_id)
    user
  end
end
