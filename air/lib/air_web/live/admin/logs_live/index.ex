defmodule AirWeb.Admin.LogsLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Schemas.Log

  @initial_entries 1_000
  @max_entries_per_refresh 100
  @refresh_interval :timer.seconds(2)

  @impl true
  def mount(_params, session, socket) do
    # prevent recursion from debug messages
    Logger.disable(self())

    today = NaiveDateTime.new!(Date.utc_today(), Time.from_seconds_after_midnight(0))
    {last_id, logs} = logs_tail(%{timestamp: today}, @initial_entries)

    socket =
      socket
      |> assign_new(:current_user, fn -> current_user!(session) end)
      |> assign(:problems, Air.Service.Warnings.problems())
      |> assign(:last_id, last_id)
      |> assign(:level, :all)
      |> assign(:source, :both)
      |> assign(:logs, logs)
      |> assign(:update_type, :replace)

    if connected?(socket), do: :timer.send_interval(@refresh_interval, self(), :refresh)

    {:ok, socket, temporary_assigns: [logs: nil]}
  end

  @impl true
  def handle_info(:refresh, socket) do
    {last_id, logs} =
      %{id: socket.assigns.last_id, level: socket.assigns.level, source: socket.assigns.source}
      |> logs_tail(@max_entries_per_refresh)

    socket = socket |> assign(:last_id, last_id) |> assign(:logs, logs) |> assign(:update_type, :append)
    {:noreply, socket}
  end

  @impl true
  def handle_event("filter", %{"level" => level, "source" => source}, socket) do
    today = NaiveDateTime.new!(Date.utc_today(), Time.from_seconds_after_midnight(0))
    level = String.to_atom(level)
    source = String.to_atom(source)

    {last_id, logs} =
      %{timestamp: today, level: level, source: source}
      |> logs_tail(@initial_entries)

    socket =
      socket
      |> assign(:last_id, last_id)
      |> assign(:level, level)
      |> assign(:source, source)
      |> assign(:logs, logs)
      |> assign(:update_type, :replace)

    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp logs_tail(filters, max_entries) do
    logs = filters |> Air.Service.Logs.tail(max_entries) |> Enum.map(&truncate_timestamp/1)
    last_id = Enum.max_by(logs, & &1.id, fn -> %{id: 0} end).id
    {last_id, logs}
  end

  defp truncate_timestamp(log), do: %Log{log | timestamp: NaiveDateTime.truncate(log.timestamp, :second)}

  defp row_class(log) do
    if rem(log.id, 2) == 0 do
      "active-row"
    else
      ""
    end
  end

  defp format_message(message),
    do: message |> String.split("\n", trim: false) |> Enum.intersperse(Phoenix.HTML.Tag.tag(:br))
end
