defmodule Cloak.DataSource.RODBC.Port do
  @moduledoc "Rust ODBC port wrapper."

  require Logger

  @command_connect 0
  @command_execute 1
  @command_fetch_rows 2
  @command_set_flag 3
  @command_get_columns 4
  @command_stop 5

  @flag_wstr_as_bin 0

  @typep row :: [any]

  use GenServer

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    Process.flag(:trap_exit, true)
    {:ok, open()}
  end

  @impl GenServer
  def handle_call({command, data}, _from, port), do: {:reply, port_control(port, command, data), port}

  @impl GenServer
  def handle_info({port, :eof}, port), do: {:stop, {:shutdown, :eof}, port}
  def handle_info({:EXIT, _from, _reason}, port), do: {:stop, :normal, port}

  def handle_info(other, port) do
    Logger.warn("Unknown message #{inspect(other)}")
    {:noreply, port}
  end

  @impl GenServer
  def terminate(:normal, port), do: close(port)
  def terminate(_, _port), do: :ok

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

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

  @doc "Enables transfer of wide strings as binary data (avoids validation & conversion of string characters)."
  @spec set_wstr_as_bin(pid()) :: :ok | {:error, String.t()}
  def set_wstr_as_bin(pid), do: call_server(pid, @command_set_flag, @flag_wstr_as_bin)

  @doc "Returns {name, type} information about the columns selected by the previous statement."
  @spec get_columns(pid()) :: {:ok, [{String.t(), String.t()}]} | {:error, String.t()}
  def get_columns(pid) do
    with {:ok, columns} <- call_server(pid, @command_get_columns, "") do
      {:ok, Enum.map(columns, &List.to_tuple/1)}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp open() do
    path = Application.app_dir(:cloak, "priv/native/rodbc") |> to_charlist()
    :erlang.open_port({:spawn_executable, path}, [:binary, :stream, :use_stdio, :eof])
  end

  defp close(port) do
    send_command(port, @command_stop, 0)
    :erlang.port_close(port)
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
        fn :ok -> :ok end
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

  defp decode_values(<<@type_f64, num::float-little-64, data::binary>>, acc),
    do: decode_values(data, [num | acc])

  defp decode_values(
         <<@type_str, len::unsigned-little-32, str::bytes-size(len), data::binary>>,
         acc
       ),
       do: decode_values(data, [str | acc])

  defp decode_values(
         <<@type_bin, len::unsigned-little-32, str::bytes-size(len), data::binary>>,
         acc
       ),
       do: decode_values(data, [str | acc])

  defp port_control(port, command, data) do
    true = send_command(port, command, data)
    read_input(port)
  end

  defp read_input(_port, buffer \\ <<>>)
  defp read_input(_port, <<size::unsigned-little-32, data::binary-size(size)>>), do: data

  defp read_input(port, buffer) do
    receive do
      {^port, {:data, data}} ->
        read_input(port, buffer <> data)

      {^port, :eof} ->
        <<@status_err, "Unexpected port eof!">>
    end
  end

  defp send_command(port, command, data) when is_binary(data) do
    :erlang.port_command(
      port,
      <<command::unsigned-little-32, byte_size(data)::unsigned-little-32, data::binary>>
    )
  end

  defp send_command(port, command, data) when is_integer(data) do
    :erlang.port_command(port, <<command::unsigned-little-32, data::unsigned-little-32>>)
  end
end
