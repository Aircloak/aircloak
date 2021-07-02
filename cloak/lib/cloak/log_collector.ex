defmodule Cloak.LogCollector do
  @moduledoc """
  Collector of Cloak logs.

  This module is a `:gen_event` handler which is installed as Logger backend.

  The handler takes all query-specific messages and sends them to the corresponding query runner process.
  It also sends all log messages to Air, when configured to do so.
  """

  @behaviour :gen_event

  # -------------------------------------------------------------------
  # :gen_event callbacks
  # -------------------------------------------------------------------

  @impl :gen_event
  def init(_arg) do
    {:ok, hostname} = :inet.gethostname()
    {:ok, %{hostname: to_string(hostname)}}
  end

  @impl :gen_event
  def handle_call(_request, _state), do: raise("invalid call")

  @impl :gen_event
  def handle_event({_level, gl, {Logger, _, _, _}}, state) when node(gl) != node(), do: {:ok, state}

  def handle_event({level, _group_leader, {Logger, message, timestamp, metadata}}, state) do
    if Application.get_env(:cloak, :send_logs_to_air) == true,
      do: Cloak.AirSocket.send_log(to_naivedatetime(timestamp), state.hostname, level, to_string(message))

    with {:ok, query_id} <- Keyword.fetch(metadata, :query_id),
         do: Cloak.Query.Runner.send_log_entry(query_id, level, message, timestamp, metadata)

    {:ok, state}
  end

  def handle_event(_other, state), do: {:ok, state}

  @impl :gen_event
  def handle_info(_msg, state), do: {:ok, state}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_naivedatetime({date, time}) do
    {hour, minute, second, millisecond} = time
    NaiveDateTime.from_erl!({date, {hour, minute, second}}, {millisecond * 1000, 6})
  end
end
