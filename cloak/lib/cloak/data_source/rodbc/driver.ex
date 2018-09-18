defmodule Cloak.DataSource.RODBC.Driver do
  @moduledoc "Rust ODBC port driver wrapper."

  require Logger

  @command_connect 0
  @command_execute 1
  @command_fetch_rows 2
  @command_set_flag 3
  @command_get_columns 4
  @command_stop 5

  @flag_wstr_as_bin 0

  @typep row :: [any]

  @doc "Creats a new port driver instance."
  @spec open() :: port()
  def open() do
    path = Application.app_dir(:cloak, "priv/native/rodbc") |> to_charlist()
    :erlang.open_port({:spawn_executable, path}, [:binary, :stream, :use_stdio, :eof])
  end

  @doc "Closes the port driver instance."
  @spec close(port()) :: boolean
  def close(port) do
    send_command(port, @command_stop, 0)
    :erlang.port_close(port)
  end

  @doc "Connects to a data source."
  @spec connect(port(), String.t()) :: :ok | {:error, String.t()}
  def connect(port, connection_string),
    do: port |> port_control(@command_connect, connection_string) |> decode_response()

  @doc "Executes an SQL statement on the connected backend."
  @spec execute(port(), String.t()) :: :ok | {:error, String.t()}
  def execute(port, statement) do
    Logger.debug(fn -> "Executing SQL query: #{statement}" end)
    port |> port_control(@command_execute, statement) |> decode_response()
  end

  @doc "Starts the streaming of rows selected by the previous statement."
  @spec start_fetching_rows(port(), pos_integer) :: boolean
  def start_fetching_rows(port, size) when size > 0, do: send_command(port, @command_fetch_rows, size)

  @doc "Returns a new batch, with the specified size, from the rows selected by the previous statement."
  @spec fetch_batch(port(), (row -> row), pos_integer) :: {:ok, Enumerable.t()} | {:error, String.t()}
  def fetch_batch(port, row_mapper, size) when size > 0 do
    case fetch_rows(port, size) do
      {:ok, []} -> {:ok, []}
      {:ok, rows} -> {:ok, Stream.map(rows, row_mapper)}
      {:error, error} -> {:error, error}
    end
  end

  @doc "Enables transfer of wide strings as binary data (avoids validation & conversion of string characters)."
  @spec set_wstr_as_bin(port()) :: :ok | {:error, String.t()}
  def set_wstr_as_bin(port),
    do: port |> port_control(@command_set_flag, @flag_wstr_as_bin) |> decode_response()

  @doc "Returns {name, type} information about the columns selected by the previous statement."
  @spec get_columns(port()) :: {:ok, [{String.t(), String.t()}]} | {:error, String.t()}
  def get_columns(port) do
    with {:ok, columns} <- port |> port_control(@command_get_columns, "") |> decode_response() do
      {:ok, Enum.map(columns, &List.to_tuple/1)}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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

  defp fetch_rows(port, size) do
    port
    |> read_input()
    |> decode_response()
    |> case do
      # end of data
      {:ok, []} ->
        {:ok, []}

      {:ok, rows} ->
        # queue next batch
        true = send_command(port, @command_fetch_rows, size)
        {:ok, rows}

      {:error, message} ->
        {:error, message}
    end
  end

  defp port_control(port, command, data) do
    true = send_command(port, command, data)
    read_input(port)
  end

  defp read_input(port) do
    receive do
      {^port, {:data, <<size::unsigned-little-32, data::binary>>}} ->
        flush_input(port, size - byte_size(data), data)

      {^port, :eof} ->
        <<@status_err, "Unexpected port eof!">>
    end
  end

  defp flush_input(_port, 0, buffer), do: buffer

  defp flush_input(port, size, buffer) do
    receive do
      {^port, {:data, data}} ->
        flush_input(port, size - byte_size(data), buffer <> data)

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
