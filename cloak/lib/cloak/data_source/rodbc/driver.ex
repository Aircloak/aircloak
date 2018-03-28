defmodule Cloak.DataSource.RODBC.Driver do
  @moduledoc "Rust ODBC port driver wrapper."

  @port_name 'librodbc'

  @command_connect 0
  @command_execute 1
  @command_fetch 2

  @typep row :: [any]

  @doc "Loads the port driver module."
  @spec init!() :: :ok
  def init!() do
    :ok =
      Application.app_dir(:cloak, "priv/native")
      |> to_charlist()
      |> :erl_ddll.load_driver(@port_name)

    :ok
  end

  @doc "Creats a new port driver instance."
  @spec open() :: port()
  def open(), do: :erlang.open_port({:spawn_driver, @port_name}, [])

  @doc "Closes the port driver instance."
  @spec close(port()) :: boolean
  def close(port), do: :erlang.port_close(port)

  @doc "Connects to a data source."
  @spec connect(port(), String.t()) :: :ok | {:error, String.t()}
  def connect(port, connection_string),
    do: port |> :erlang.port_control(@command_connect, connection_string) |> decode_response()

  @doc "Executes an SQL statement on the connected backend."
  @spec execute(port(), String.t()) :: :ok | {:error, String.t()}
  def execute(port, statement), do: port |> :erlang.port_control(@command_execute, statement) |> decode_response()

  @doc "Returns all rows selected by the previous statement."
  @spec fetch_all(port(), (row -> row)) :: {:ok, [row]} | {:error, String.t()}
  def fetch_all(port, row_mapper), do: fetch_all(port, row_mapper, [])

  @doc "Returns a new batch, with the specified size, from the rows selected by the previous statement."
  @spec fetch_batch(port(), (row -> row), pos_integer) :: {:ok, [row]} | {:error, String.t()}
  def fetch_batch(port, row_mapper, size) when size > 0, do: fetch_batch(port, row_mapper, size, [], 0)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @type_null 0
  @type_i32 1
  @type_i64 2
  @type_f32 3
  @type_f64 4
  @type_bin 5

  @status_err ?E
  @status_ok ?K

  defp decode_response(""), do: :ok
  defp decode_response(<<@status_err, message::binary>>), do: {:error, message}
  defp decode_response(<<@status_ok, data::binary>>), do: {:ok, decode_data(data, [])}

  defp decode_data(<<>>, acc), do: :lists.reverse(acc)
  defp decode_data(<<@type_null, data::binary>>, acc), do: decode_data(data, [nil | acc])

  defp decode_data(<<@type_i32, num::signed-little-32, data::binary>>, acc), do: decode_data(data, [num | acc])

  defp decode_data(<<@type_i64, num::signed-little-64, data::binary>>, acc), do: decode_data(data, [num | acc])

  defp decode_data(<<@type_f32, num::float-little-32, data::binary>>, acc), do: decode_data(data, [num | acc])

  defp decode_data(<<@type_f64, num::float-little-64, data::binary>>, acc), do: decode_data(data, [num | acc])

  defp decode_data(
         <<@type_bin, len::unsigned-little-32, str::bytes-size(len), data::binary>>,
         acc
       ),
       do: decode_data(data, [str | acc])

  defp fetch_row(port), do: port |> :erlang.port_control(@command_fetch, "") |> decode_response()

  defp fetch_all(port, row_mapper, acc) do
    case fetch_row(port) do
      :ok -> {:ok, :lists.reverse(acc)}
      {:ok, row} -> fetch_all(port, row_mapper, [row_mapper.(row) | acc])
      {:error, error} -> {:error, error}
    end
  end

  defp fetch_batch(_port, _row_mapper, size, acc, size), do: {:ok, :lists.reverse(acc)}

  defp fetch_batch(port, row_mapper, size, acc, count) do
    case fetch_row(port) do
      :ok -> {:ok, :lists.reverse(acc)}
      {:ok, row} -> fetch_batch(port, row_mapper, size - 1, [row_mapper.(row) | acc], count + 1)
      {:error, error} -> {:error, error}
    end
  end
end
