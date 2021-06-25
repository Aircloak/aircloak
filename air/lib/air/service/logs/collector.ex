defmodule Air.Service.Logs.Collector do
  @moduledoc """
  Collector of Air logs.

  This module is a `:gen_event` handler which is installed as Logger backend.
  The handler takes all messages and saves them to the database.
  """

  @behaviour :gen_event

  # -------------------------------------------------------------------
  # :gen_event callbacks
  # -------------------------------------------------------------------

  @impl :gen_event
  def init(_arg) do
    # prevent recursion
    Logger.disable(self())

    {:ok, hostname} = :inet.gethostname()
    {:ok, %{hostname: to_string(hostname)}}
  end

  @impl :gen_event
  def handle_call(_request, _state), do: raise("invalid call")

  @impl :gen_event
  def handle_event({_level, gl, {Logger, _, _, _}}, state) when node(gl) != node(), do: {:ok, state}

  def handle_event({level, _group_leader, {Logger, message, {date, time}, _metadata}}, state) do
    {hour, minute, second, millisecond} = time
    timestamp = NaiveDateTime.from_erl!({date, {hour, minute, second}}, {millisecond * 1000, 6})
    Air.Service.Logs.save(state.hostname, :air, timestamp, to_string(["[#{level}] " | message]))
    {:ok, state}
  end

  def handle_event(_other, state), do: {:ok, state}

  @impl :gen_event
  def handle_info(_msg, state), do: {:ok, state}
end
