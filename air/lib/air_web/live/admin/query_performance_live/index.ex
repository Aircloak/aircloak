defmodule AirWeb.Admin.QueryPerformanceLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Service.Query

  @impl true
  def mount(_params, session, socket) do
    {
      :ok,
      socket
      |> assign_new(:current_user, fn -> current_user!(session) end)
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
      data_sources: params["data_sources"] || [],
      query_states: [:completed],
      max_results: 1000
    }

    {
      :noreply,
      socket
      |> assign(:interesting_queries, Query.performance_interesting_queries(filters))
      |> assign(:histogram, Query.performance_histogram(filters))
      |> assign(
        :users,
        Query.users_for_filters(%{filters | users: [], data_sources: []})
        |> Enum.map(&%{label: &1.name, value: &1.id})
      )
      |> assign(
        :data_sources,
        Query.data_sources_for_filters(%{filters | users: [], data_sources: []})
        |> Enum.map(&%{label: &1.name, value: &1.id})
      )
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
         admin_query_performance_index_path(
           socket,
           :index,
           Map.merge(
             socket.assigns.query_params,
             %{from: params["from"], to: params["to"]}
           )
         )
     )}
  end

  @color_map %{
    "waiting for database" => "#ff9da6",
    "compiling" => "#4b78a8",
    "completed" => "#e45756",
    "ingesting data" => "#e45857",
    "parsing" => "#f58518",
    "post-processing" => "#54a24b",
    "processing" => "#eeca3b",
    "started" => "#b279a2"
  }

  defp get_color(phase), do: Map.get(@color_map, phase, "black")

  defp render_query_details(assigns) do
    ~L"""
    <div class="query-details">
      <p><strong>Execution time:</strong> <%= Float.round(@total_time / 1000, 2) %>s</p>
      <div class="progress">
        <%= for {phase, dur} <- sort_phases(@time_spent) do %>
          <div class="progress-bar" style="width: <%= dur / @total_time * 100 %>%; background-color: <%= get_color(phase) %>" <%= if dur / @total_time <= 0.15 do %>title="<%= phase %> (<%= Float.round(dur / 1000, 1) %>s)"<% end %>>
            <%= if dur / @total_time > 0.15 do %>
              <%= phase %> (<%= Float.round(dur / 1000, 1) %>s)
            <% end %>
          </div>
        <% end %>
      </div>
      <sql-code-block phx-update="ignore" code="<%= @statement %>"></sql-code-block>
      <div class="d-flex justify-content-between mt-2">
        <span class="flex-basis-1 flex-grow-1"><i class="fas fa-user" aria-label="User"></i> <%= @user_name %></span>
        <span class="flex-basis-1 flex-grow-1 text-center"><i class="fas fa-database" aria-label="Data source"></i> <%= @data_source_name %></span>
        <span class="flex-basis-1 flex-grow-1 text-right"><i class="fas fa-stopwatch" aria-label="Started"></i> <%= @inserted_at %></span>
      </div>
    </div>
    """
  end

  @sort_order %{
    "started" => 1,
    "parsing" => 2,
    "compiling" => 3,
    "waiting for database" => 4,
    "ingesting data" => 5,
    "processing" => 6,
    "post-processing" => 7,
    "completed" => 8
  }
  defp sort_phases(phases) do
    phases |> Enum.sort_by(fn {key, _val} -> @sort_order[key] end)
  end

  defp parse_datetime(value, default) do
    case Timex.parse(value, "{ISOdate} {ISOtime}") do
      {:ok, result} -> result
      _error -> default
    end
  end
end
