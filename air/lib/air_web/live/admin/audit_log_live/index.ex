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

  defp render_audit_log(assigns) do
    ~L"""
    <div class="d-flex mb-3 align-self-stretch ml-3">
      <time class="timestamp small flex-shrink-0 text-muted" datetime="<%= NaiveDateTime.to_iso8601(@timestamp) %>">
        <%= Timex.format!(@timestamp, "{h24}:{m}:{s}") %><br />
      </time>
      <%= icon_for_event(@event) %>
      <div class="w-100">
        <p>
          <%= live_patch @user_name, to: admin_audit_log_index_path(@socket, :index, Map.put(@query_params, :users, [@user_id])) %>
          <strong><%= live_patch @event, to: admin_audit_log_index_path(@socket, :index, Map.put(@query_params, :events, [@event])), class: "text-dark" %></strong>
          <%= if @metadata["data_source"], do: "on #{@metadata["data_source"]}" %>
          <br />
          <%= if @metadata["remote_ip"] && @metadata["remote_ip"] do %>
            <small>IP Address: <%= @metadata["remote_ip"] %> Peer: <%= @metadata["peer"] %></small>
          <% end %>
        </p>

        <%= for {key, val} <- Map.drop(@metadata, ["remote_ip", "peer", "data_source"]) do %>
          <%= if key == "query" do %>
            <pre phx-hook="CodeViewer"><%= val %></pre>
          <% else %>
          <div class="d-flex">
            <strong><%= key %></strong>
            <span><%= val %></span>
          </div>
          <% end %>
        <% end %>
      </div>
    </div>
    """
  end

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

  defp icon_for_event(event) do
    content_tag(:span, class: "fa-stack audit-icon", style: "color: #{icon_color(event)}") do
      [
        content_tag(:i, "", class: "fas fa-circle fa-stack-2x"),
        content_tag(:i, "", class: "fas fa-stack-1x #{icon_class(event)} fa-inverse")
      ]
    end
  end

  defp icon_setup("Executed query"), do: {"red", "fa-terminal"}
  defp icon_setup("Logged in"), do: {"green", "fa-sign-in-alt"}
  defp icon_setup("Altered Diffix Explorer permissions"), do: {"purple", "fa-compass"}

  defp event_hue(event) do
    cond do
      String.contains?(event, "clear") || String.contains?(event, "remov") || String.contains?(event, "fail") ->
        # reds
        {0, 30}

      String.contains?(event, "alter") || String.contains?(event, "chang") || String.contains?(event, "updat") ->
        # yellows
        {30, 70}

      String.contains?(event, "creat") ->
        # greens
        {70, 160}

      String.contains?(event, "disabl") ||
          String.contains?(event, "delet") ->
        # reds
        {320, 360}

      true ->
        # blues
        {160, 320}
    end
  end

  defp format_date_header(time) do
    case Timex.diff(DateTime.utc_now(), time, :days) do
      0 -> "Today"
      1 -> "Yesterday"
      _ -> Timex.format!(time, "{D} {Mshort} {YYYY}")
    end
  end

  defp current_user!(%{"_air_session_token" => token}) do
    {:ok, user_id} = Air.Service.RevokableToken.verify(token, :session)
    {:ok, user} = Air.Service.User.load_enabled(user_id)
    user
  end
end
