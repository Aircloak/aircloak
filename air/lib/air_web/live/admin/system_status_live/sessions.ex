defmodule AirWeb.Admin.SystemStatusLive.Sessions do
  @moduledoc false
  use Air.Web, :live_view

  @default_num_events 3

  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @impl true
  def mount(_params, _session, socket) do
    AirWeb.Endpoint.subscribe("login_events")

    {
      :ok,
      socket
      |> assign(:show_more, false)
      |> assign(:all_events, Air.Service.User.LoginStats.get_stats())
      |> compute_display_state()
    }
  end

  @impl true
  def handle_event("toggle_more", _value, socket) do
    {:noreply,
     socket
     |> assign(:show_more, not socket.assigns.show_more)
     |> compute_display_state()}
  end

  @impl true
  def handle_info(%{topic: "login_events", event: "update", payload: %{login_events: events}}, socket) do
    {:noreply,
     socket
     |> assign(:all_events, events)
     |> compute_display_state()}
  end

  # -------------------------------------------------------------------
  # Invoked from template
  # -------------------------------------------------------------------

  def default_num_events(), do: @default_num_events

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compute_display_state(socket) do
    all_events = socket.assigns.all_events
    successes = all_events |> Enum.filter(&(&1.type == :success)) |> Enum.count()
    failures = all_events |> Enum.filter(&(&1.type == :failure)) |> Enum.count()

    socket
    |> assign(
      :events_to_show,
      if socket.assigns.show_more do
        all_events
      else
        Enum.take(all_events, @default_num_events)
      end
    )
    |> assign(:successes, successes)
    |> assign(:failures, failures)
  end
end
