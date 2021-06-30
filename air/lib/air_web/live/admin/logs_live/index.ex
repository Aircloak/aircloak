defmodule AirWeb.Admin.LogsLive.Index do
  @moduledoc false
  use Air.Web, :live_view

  alias Air.Schemas.Log

  @max_entries_per_refresh 1_000
  @refresh_interval :timer.seconds(2)

  @impl true
  def mount(_params, session, socket) do
    # prevent recursion from debug messages
    Logger.disable(self())

    {last_timestamp, logs} = NaiveDateTime.new!(Date.utc_today(), Time.from_seconds_after_midnight(0)) |> logs_tail()

    if connected?(socket), do: :timer.send_interval(@refresh_interval, self(), :refresh)

    {
      :ok,
      socket
      |> assign_new(:current_user, fn -> current_user!(session) end)
      |> assign(:problems, Air.Service.Warnings.problems())
      |> assign(:last_timestamp, last_timestamp)
      |> assign(:logs, logs),
      temporary_assigns: [logs: []]
    }
  end

  @impl true
  def handle_info(:refresh, socket) do
    {last_timestamp, logs} = socket.assigns.last_timestamp |> logs_tail()
    socket = socket |> assign(:last_timestamp, last_timestamp) |> assign(:logs, logs)
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp logs_tail(since) do
    case Air.Service.Logs.tail(since, @max_entries_per_refresh) do
      [] ->
        {since, []}

      [%Log{timestamp: last_timestamp} | _] = logs ->
        {last_timestamp, logs |> Enum.map(&truncate_timestamp/1)}
    end
  end

  defp truncate_timestamp(log), do: %Log{log | timestamp: NaiveDateTime.truncate(log.timestamp, :second)}
end
