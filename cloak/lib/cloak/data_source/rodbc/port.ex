defmodule Cloak.DataSource.RODBC.Port do
  @moduledoc "Rust ODBC port wrapper."

  require Logger

  @command_connect 0
  @command_execute 1
  @command_fetch_rows 2
  @command_set_flag 3
  @command_get_columns 4
  @command_stop 5

  @flag_wstr_as_utf16 0

  @typep row :: [any]

  use GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the port owner process."
  @spec start_link() :: GenServer.on_start()
  def start_link(), do: GenServer.start_link(__MODULE__, nil)

  @doc "Stops the port owner process."
  @spec stop(pid()) :: :ok
  def stop(pid), do: GenServer.stop(pid)

  @doc "Connects to a data source."
  @spec connect(pid(), String.t(), timeout()) :: :ok | {:error, String.t()}
  def connect(pid, connection_string, timeout),
    do: call_server(pid, @command_connect, connection_string, timeout)

  @doc "Executes an SQL statement on the connected backend."
  @spec execute(pid(), String.t(), timeout()) :: :ok | {:error, String.t()}
  def execute(pid, statement, timeout \\ :infinity) do
    Logger.debug(fn -> "Executing SQL query: #{statement}" end)
    call_server(pid, @command_execute, statement, timeout)
  end

  @doc "Returns a new batch, with the specified size, from the rows selected by the previous statement."
  @spec fetch_batch(pid(), (row -> row), pos_integer) :: {:ok, Enumerable.t()} | {:error, String.t()}
  def fetch_batch(pid, row_mapper, size) when size > 0 do
    case call_server(pid, @command_fetch_rows, size, :timer.minutes(30)) do
      {:ok, []} ->
        {:ok, []}

      {:ok, rows} ->
        {:ok, Stream.map(rows, row_mapper)}

      {:error, error} ->
        {:error, error}
    end
  end

  @doc "Transfers wide strings encoded as UTF-16 (for ODBC drivers that don't support UTF-8)."
  @spec set_wstr_as_utf16(pid()) :: :ok | {:error, String.t()}
  def set_wstr_as_utf16(pid), do: call_server(pid, @command_set_flag, @flag_wstr_as_utf16)

  @doc "Returns {name, type} information about the columns selected by the previous statement."
  @spec get_columns(pid()) :: {:ok, [{String.t(), String.t()}]} | {:error, String.t()}
  def get_columns(pid) do
    with {:ok, columns} <- call_server(pid, @command_get_columns, "") do
      {:ok, Enum.map(columns, &List.to_tuple/1)}
    end
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    # We trap exits because we want to shutdown forcefully, in case the port is not responding in a timely manner.
    Process.flag(:trap_exit, true)
    {:ok, %{port: open(), client: nil, buffer: nil}}
  end

  @impl GenServer
  def handle_call({command, data}, from, %{port: port, client: nil, buffer: nil} = state) do
    true = send_command(port, command, data)
    # Command sent, now we wait for the port reply.
    {:noreply, %{state | client: from, buffer: <<>>}}
  end

  @impl GenServer
  def handle_info({port, :eof}, %{port: port}), do: raise("RODBC port terminated unexpectedly.")
  def handle_info({:EXIT, _process, _reason}, state), do: {:stop, :normal, state}

  def handle_info({port, {:data, data}}, %{port: port, buffer: buffer} = state) when is_binary(buffer) do
    state =
      case buffer <> data do
        <<size::unsigned-little-32, data::binary-size(size)>> ->
          # The response is complete, we finish the previous call by sending it to the waiting client.
          GenServer.reply(state.client, data)
          %{state | client: nil, buffer: nil}

        buffer ->
          # The response is incomplete, we need to wait for more data.
          %{state | buffer: buffer}
      end

    {:noreply, state}
  end

  @impl GenServer
  def terminate({%RuntimeError{}, _stack_trace}, _state), do: :ok

  @shutdown_timeout :timer.seconds(2)
  def terminate(_reason, %{port: port}) do
    {:os_pid, os_pid} = :erlang.port_info(port, :os_pid)
    send_command(port, @command_stop, 0)

    receive do
      {^port, :eof} -> :ok
    after
      @shutdown_timeout -> "kill #{os_pid}" |> to_charlist() |> :os.cmd()
    end

    :erlang.port_close(port)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp open() do
    path = Application.app_dir(:cloak, "priv/native/rodbc") |> to_charlist()
    :erlang.open_port({:spawn_executable, path}, [:binary, :stream, :use_stdio, :eof])
  end

  defp call_server(pid, command, data, timeout \\ :timer.seconds(30)) do
    pid
    |> GenServer.call({command, data}, timeout)
    |> decode_response()
  end

  @type_null 0
  @type_i32 1
  @type_i64 2
  @type_f32 3
  @type_f64 4
  @type_str 5
  @type_bin 6
  @type_wstr 7

  @status_err ?E
  @status_ok ?K
  @status_row ?R
  @status_bin ?B
  @status_table ?T

  defp decode_response(<<@status_ok>>), do: :ok
  defp decode_response(<<@status_err, message::binary>>), do: {:error, message}
  defp decode_response(<<@status_bin, data::binary>>), do: {:ok, data}
  defp decode_response(<<@status_row, data::binary>>), do: {:ok, decode_values(data, [])}

  defp decode_response(<<@status_table, _columns_count::unsigned-little-32>>), do: {:ok, []}

  defp decode_response(<<@status_table, columns_count::unsigned-little-32, data::binary>>) do
    stream =
      Stream.resource(
        fn -> data end,
        fn
          :ok -> {:halt, :ok}
          data -> {decode_values(data, []), :ok}
        end,
        fn _ -> :ok end
      )
      |> Stream.chunk_every(columns_count)

    {:ok, stream}
  end

  defp decode_values(<<>>, acc), do: :lists.reverse(acc)
  defp decode_values(<<@type_null, data::binary>>, acc), do: decode_values(data, [nil | acc])

  defp decode_values(<<@type_i32, num::signed-little-32, data::binary>>, acc),
    do: decode_values(data, [num | acc])

  defp decode_values(<<@type_i64, num::signed-little-64, data::binary>>, acc),
    do: decode_values(data, [num | acc])

  defp decode_values(<<@type_f32, num::float-little-32, data::binary>>, acc),
    do: decode_values(data, [num | acc])

  defp decode_values(<<@type_f32, _::32, data::binary>>, acc),
    do: decode_values(data, [nil | acc])

  defp decode_values(<<@type_f64, num::float-little-64, data::binary>>, acc),
    do: decode_values(data, [num | acc])

  defp decode_values(<<@type_f64, _::64, data::binary>>, acc),
    do: decode_values(data, [nil | acc])

  defp decode_values(<<@type_str, len::unsigned-little-32, str::bytes-size(len), data::binary>>, acc),
    do: decode_values(data, [str | acc])

  defp decode_values(<<@type_bin, len::unsigned-little-32, str::bytes-size(len), data::binary>>, acc),
    do: decode_values(data, [str | acc])

  defp decode_values(<<@type_wstr, len::unsigned-little-32, str::bytes-size(len), data::binary>>, acc) do
    str = :unicode.characters_to_binary(str, {:utf16, :little}, :utf8)
    decode_values(data, [str | acc])
  end

  defp send_command(port, command, data) when is_binary(data) do
    :erlang.port_command(port, <<command::unsigned-little-32, byte_size(data)::unsigned-little-32, data::binary>>)
  end

  defp send_command(port, command, data) when is_integer(data) do
    :erlang.port_command(port, <<command::unsigned-little-32, data::unsigned-little-32>>)
  end
end
