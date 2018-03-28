defmodule Aircloak do
  @moduledoc false
  use Application
  require Logger

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [supervisor(Aircloak.ProcessMonitor, [])],
      strategy: :one_for_one,
      name: Aircloak.Supervisor
    )
  end

  @doc "Recursively atomizes the keys in a map, leaving values untouched."
  @spec atomize_keys(Map.t()) :: Map.t()
  def atomize_keys(%{} = map) do
    for {key, value} <- map, into: %{}, do: {String.to_atom(key), atomize_keys(value)}
  end

  def atomize_keys(list) when is_list(list) do
    Enum.map(list, &atomize_keys/1)
  end

  def atomize_keys(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> atomize_keys()
    |> List.to_tuple()
  end

  def atomize_keys(other), do: other

  @doc """
  Executes the function, logs the running time, and returns the function result.

  Options:

    - `:threshold` - The minimum running time in milliseconds required to make a log entry. Defaults to 0.
    - `:level` - Logger level used to make a log entry. Defaults to `:info`.
  """
  @spec measure(
          any,
          (() -> result),
          threshold: non_neg_integer,
          level: :debug | :info | :warn | :error
        ) :: result
        when result: var
  def measure(id, fun, opts \\ []) do
    {time, result} = :timer.tc(fun)

    if time >= Keyword.get(opts, :threshold, 0) * 1000 do
      Logger.log(
        Keyword.get(opts, :level, :info),
        "operation `#{inspect(id)}` took #{div(time, 1000)}ms"
      )
    end

    result
  end

  @doc """
  Executes the operation, and logs the running time if it exceeds some threshold.

  The `:threshold` option can be used to set the threshold in milliseconds. The default value is `10`.
  """
  @spec report_long(any, (() -> result), threshold: non_neg_integer) :: result when result: var
  def report_long(id, fun, opts \\ []), do: measure(id, fun, level: :warn, threshold: Keyword.get(opts, :threshold, 10))

  @doc "Waits for the service on the given host/port to become available."
  @spec await_service!(String.t(), :inet.port_number()) :: :ok
  def await_service!(host, port) do
    task =
      Task.async(fn ->
        fn -> :gen_tcp.connect(to_charlist(host), port, []) end
        |> Stream.repeatedly()
        |> Stream.drop_while(&match?({:error, _}, &1))
        |> Enum.take(1)
      end)

    case Task.yield(task, :timer.minutes(2)) do
      nil -> raise("#{host}:#{port} is not available")
      _ -> :ok
    end
  end
end
