defmodule AirWeb.Admin.AuditLogLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Service.AuditLog

  @page_size 50

  @impl true
  def mount(_params, session, socket) do
    {
      :ok,
      socket
      |> assign(:expanded_logs, %{})
      |> assign_new(:current_user, fn -> current_user!(session) end),
      temporary_assigns: [expanded_logs: %{}]
    }
  end

  @impl true
  def handle_params(params, _uri, socket) do
    from = parse_datetime(params["from"], Timex.now() |> Timex.shift(months: -1))
    to = parse_datetime(params["to"], Timex.now())

    filters = %{
      from: from,
      to: to,
      users: params["users"] || [],
      events: params["events"] || [],
      data_sources: params["data_sources"] || [],
      except: [],
      max_results: @page_size,
      page: 1
    }

    {
      :noreply,
      socket
      |> assign(:page, 1)
      |> assign(:expanded, %{})
      |> assign(:users, AuditLog.users(filters) |> Enum.map(&%{label: &1.name, value: &1.id}))
      |> assign(:event_types, AuditLog.event_types(filters) |> Enum.map(&%{label: &1, value: &1}))
      |> assign(:data_sources, AuditLog.data_sources(filters) |> Enum.map(&%{label: &1.name, value: &1.name}))
      |> assign(:from, from)
      |> assign(:to, to)
      |> assign(:query_params, params)
      |> then_try(fn socket ->
        {more_pages, audit_logs} = AuditLog.grouped_for(filters)

        socket
        |> assign(:more_pages, more_pages)
        |> assign(:audit_logs, audit_logs)
      end)
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

  def handle_event("load_next_page", _params, socket) do
    page = socket.assigns.page + 1
    params = socket.assigns.query_params

    filters = %{
      from: socket.assigns.from,
      to: socket.assigns.to,
      users: params["users"] || [],
      events: params["events"] || [],
      data_sources: params["data_sources"] || [],
      except: [],
      max_results: @page_size,
      page: page
    }

    {
      :noreply,
      socket
      |> assign(:page, page)
      |> then_try(fn socket ->
        {more_pages, audit_logs} = AuditLog.grouped_for(filters)

        socket
        |> assign(:more_pages, more_pages)
        |> assign(:audit_logs, AuditLog.merged_grouped_lists(socket.assigns.audit_logs, audit_logs))
      end)
    }
  end

  def handle_event("expand_section", params, socket) do
    from = Timex.parse!(params["from"], "{ISOdate} {ISOtime}")
    to = Timex.parse!(params["to"], "{ISOdate} {ISOtime}")
    {user, _} = Integer.parse(params["user"])
    {page, _} = Integer.parse(params["page"])

    data_sources = socket.assigns.query_params["data_sources"]

    filters = %{
      from: from,
      to: to,
      users: [user],
      events: [params["event"]],
      data_sources: data_sources || [],
      max_results: @page_size,
      except: [params["first-event"]],
      page: page
    }

    {more_pages, audit_logs} = AuditLog.for(filters)

    expanded =
      Map.put(socket.assigns.expanded, {params["event"], user, to}, %{
        page: page,
        loaded: 1 + page * @page_size,
        has_more: more_pages
      })

    {:noreply,
     socket
     |> assign(:expanded, expanded)
     |> assign(:expanded_logs, %{{params["event"], user, to} => audit_logs})}
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
    <div id="audit-log-<%= @id %>" class="d-flex mb-3 align-self-stretch ml-3">
      <time class="timestamp small flex-shrink-0 text-muted" datetime="<%= NaiveDateTime.to_iso8601(@timestamp) %>">
        <%= Timex.format!(@timestamp, "{h24}:{m}:{s}") %><br />
      </time>
      <%= icon_for_event(@event) %>
      <div class="w-100">
        <p>
          <%= live_patch @user_name, to: admin_audit_log_index_path(@socket, :index, Map.put(@query_params, :users, [@user_id])) %>
          <strong><%= live_patch @event, to: admin_audit_log_index_path(@socket, :index, Map.put(@query_params, :events, [@event])), class: "text-dark" %></strong>
          <%= if @metadata["data_source"], do: "on #{@metadata["data_source"]}" %>
          <%= if @metadata["group_name"], do: link(@metadata["group_name"], to: admin_group_path(@socket, :edit, @metadata["group_id"])) %>
          <%= if @metadata["user"], do: @metadata["user"] %>
          <br />
          <%= if @metadata["remote_ip"] && @metadata["remote_ip"] do %>
            <small>IP Address: <%= @metadata["remote_ip"] %> Peer: <%= @metadata["peer"] %></small>
          <% end %>
        </p>

        <%= if @metadata["diff"] do %>
          <%= for {key, %{"before" => before, "after" => aftr}} <- @metadata["diff"] do %>
            <div>
              <strong><%= key %>:</strong>
              <span class="before" aria-label="Previous value"><%= before %></span>
              <i class="fas fa-arrow-right"></i>
              <span class="after" aria-label="Current value"><%= aftr %></span>
            </div>
          <% end %>
        <% end %>

        <%= if @metadata["query"] do %><pre phx-hook="SQLCodeViewer"><%= @metadata["query"] %></pre><% end %>

        <%= for {key, val} <- Map.drop(@metadata, ["remote_ip", "peer", "data_source", "diff", "group_id", "group_name", "query", "user"]) do %>
          <div class="d-flex">
            <strong style="width: 150px"><%= event_key_for_display(key) %></strong>
            <span><%= val %></span>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp then_try(socket, fun) do
    fun.(socket)
  rescue
    DBConnection.ConnectionError ->
      socket
      |> assign(:audit_logs, [])
      |> assign(:more_pages, false)
      |> assign(:expanded, %{})
      |> assign(:page, 1)
      |> put_flash(
        :error,
        "Could not retrieve data due to high database load. Try narrowing the filters to get fewer results."
      )
  end

  defp event_key_for_display(key) when is_atom(key), do: key |> Atom.to_string() |> event_key_for_display()
  defp event_key_for_display(key), do: key |> String.replace("_", " ") |> String.capitalize()

  defp parse_datetime(value, default) do
    case Timex.parse(value, "{ISOdate} {ISOtime}") do
      {:ok, result} ->
        result

      _error ->
        default
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

  defp icon_class("Executed query"), do: "fa-terminal"
  defp icon_class("Logged in"), do: "fa-sign-in-alt"
  defp icon_class("Failed login"), do: "fa-sign-in-alt"
  defp icon_class("Logged out"), do: "fa-sign-out-alt"
  defp icon_class("Added table to Diffix Explorer"), do: "fa-compass"
  defp icon_class("Removed table from Diffix Explorer"), do: "fa-compass"
  defp icon_class("Updated settings"), do: "fa-cog"
  defp icon_class("Cancelled query"), do: "fa-ban"
  defp icon_class("Deleted query"), do: "fa-trash"
  defp icon_class("Altered group"), do: "fa-users"
  defp icon_class("Created API token"), do: "fa-check-circle"
  defp icon_class("Invalidated API token"), do: "fa-times-circle"
  defp icon_class("Created app login"), do: "fa-mobile"
  defp icon_class("Revoked app login"), do: "fa-mobile-alt"
  defp icon_class("Password reset"), do: "fa-unlock"
  defp icon_class("Altered their profile"), do: "fa-id-card-alt"
  defp icon_class("Changed their password"), do: "fa-key"
  defp icon_class("Cleared their sessions"), do: "fa-lock"
  defp icon_class("Created group"), do: "fa-user-friends"
  defp icon_class("Removed group"), do: "fa-users-slash"
  defp icon_class("Created user"), do: "fa-user-plus"
  defp icon_class("User created"), do: "fa-user-plus"
  defp icon_class("Altered a user"), do: "fa-user-edit"
  defp icon_class("User altered"), do: "fa-user-edit"
  defp icon_class("Disabled a user account prior to deletion"), do: "fa-user-shield"
  defp icon_class("Scheduled user for removal"), do: "fa-user-slash"
  defp icon_class("Succeeded in removing user"), do: "fa-user-times"
  defp icon_class("Failed in removing user"), do: "fa-user-minus"
  defp icon_class("Disabled a user account"), do: "fa-user-shield"
  defp icon_class("Cleared user sessions"), do: "fa-lock"
  defp icon_class("Enabled a user account"), do: "fa-user-check"
  defp icon_class("Altered data source"), do: "fa-database"
  defp icon_class(_other), do: "fa-circle"

  defp icon_color(event) do
    <<value::224>> = :crypto.hash(:sha224, event)
    {min_hue, max_hue} = event_hue(String.downcase(event))
    h = rem(value, 727) * (max_hue - min_hue) / 727 + min_hue
    value = floor(floor(value / 360) / 3)
    s = Enum.at([0.5, 0.8, 0.9], rem(value, 3))
    value = floor(value / 3)
    l = Enum.at([0.55, 0.6, 0.75], rem(value, 3))
    "hsl(#{h},#{s * 100}%,#{l * 100}%)"
  end

  defp event_hue(event) do
    cond do
      String.contains?(event, ["clear", "remov", "fail"]) ->
        # reds
        {0, 30}

      String.contains?(event, ["alter", "chang", "updat", "add"]) ->
        # yellows
        {30, 70}

      String.contains?(event, "creat") ->
        # greens
        {70, 160}

      String.contains?(event, ["disabl", "delet"]) ->
        # reds
        {320, 360}

      true ->
        # blues
        {160, 320}
    end
  end

  defp format_date_header(time) do
    case Date.diff(Date.utc_today(), NaiveDateTime.to_date(time)) do
      0 -> "Today"
      1 -> "Yesterday"
      _ -> Timex.format!(time, "{D} {Mshort} {YYYY}")
    end
  end

  defp fetch_page_data(expanded, audit_log, key, default) do
    get_in(expanded, [{audit_log.event, audit_log.user_id, audit_log.max_date}, key]) || default
  end

  defp show_pagination?(expanded, audit_log) do
    audit_log.occurences > 1 &&
      (not Map.has_key?(expanded, {audit_log.event, audit_log.user_id, audit_log.max_date}) ||
         fetch_page_data(expanded, audit_log, :has_more, false))
  end
end
