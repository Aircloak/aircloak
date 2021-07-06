defmodule Cloak.LogCollector do
  @moduledoc "Collect, dispatch and buffer Cloak logs."

  alias Cloak.AirSocket

  use GenServer, start: {__MODULE__, :start_link, []}

  @max_buffered_entries 100

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the log collector server and logger backend."
  @spec start_link() :: GenServer.on_start()
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc "Starts buffering log messages."
  @spec start_buffering :: :ok
  def start_buffering(), do: GenServer.call(__MODULE__, :start_buffering)

  @doc "Flushes buffered log messages to Air and sets the mode to passthrough."
  @spec stop_buffering :: :ok
  def stop_buffering(), do: GenServer.call(__MODULE__, :stop_buffering)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    Logger.add_backend(__MODULE__.LoggerBackend, flush: true)
    {:ok, hostname} = :inet.gethostname()
    {:ok, %{buffer: [], mode: :buffer, hostname: to_string(hostname)}}
  end

  @impl GenServer

  def handle_call(:start_buffering, _from, state) do
    {:reply, :ok, %{state | mode: :buffer}}
  end

  def handle_call(:stop_buffering, _from, state) do
    for {timestamp, level, message} <- Enum.reverse(state.buffer),
        do: AirSocket.send_log(timestamp, state.hostname, level, message)

    if length(state.buffer) == @max_buffered_entries do
      AirSocket.send_log(
        NaiveDateTime.local_now(),
        state.hostname,
        :warn,
        "Cloak log buffer was full, some entries were dropped while disconnected from Air!"
      )
    end

    {:reply, :ok, %{state | buffer: [], mode: :passthrough}}
  end

  @impl GenServer

  def handle_cast({:dispatch, timestamp, level, message}, %{mode: :buffer, buffer: buffer} = state) do
    buffer =
      if length(buffer) == @max_buffered_entries,
        do: state.buffer,
        else: [{timestamp, level, message} | state.buffer]

    {:noreply, %{state | buffer: buffer}}
  end

  def handle_cast({:dispatch, timestamp, level, message}, %{mode: :passthrough} = state) do
    AirSocket.send_log(timestamp, state.hostname, level, message)
    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Logger backend
  # -------------------------------------------------------------------

  defmodule LoggerBackend do
    @moduledoc """
    Handler of Cloak logs.

    This module is a `:gen_event` handler which is installed as Logger backend.

    The handler takes all query-specific messages and sends them to the corresponding query runner process.
    It also sends all log messages to the log buffer, when configured to do so.
    """

    @behaviour :gen_event

    # -------------------------------------------------------------------
    # :gen_event callbacks
    # -------------------------------------------------------------------

    @impl :gen_event
    def init(_arg), do: {:ok, nil}

    @impl :gen_event
    def handle_call(_request, _state), do: raise("invalid call")

    @impl :gen_event
    def handle_event({_level, gl, {Logger, _, _, _}}, state) when node(gl) != node(), do: {:ok, state}

    def handle_event({level, _group_leader, {Logger, message, timestamp, metadata}}, state) do
      if Application.get_env(:cloak, :send_logs_to_air) == true,
        do: GenServer.cast(Cloak.LogCollector, {:dispatch, to_naivedatetime(timestamp), level, to_string(message)})

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
end
