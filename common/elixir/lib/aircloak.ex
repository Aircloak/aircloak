defmodule Aircloak do
  @moduledoc false
  use Application
  require Logger

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [supervisor(Aircloak.ProcessMonitor, [])],
      strategy: :one_for_one, name: Aircloak.Supervisor
    )
  end

  @doc "Recursively atomizes the keys in a map, leaving values untouched."
  @spec atomize_keys(Map.t) :: Map.t
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

    - `:threshold` - The minimum running time (microseconds) required to make a log entry. Defaults to `0` (10 ms).
    - `:level` - Logger level used to make a log entry. Defaults to `:info`.
  """
  @spec measure(any, (() -> result), [threshold: non_neg_integer, level: :debug | :info | :warn | :error]) :: result
    when result: var
  def measure(id, fun, opts \\ []) do
    {time, result} = :timer.tc(fun)

    if time >= Keyword.get(opts, :threshold, 0) do
      Logger.log(
        Keyword.get(opts, :level, :info),
        "operation `#{inspect id}` took #{div(time, 1000)}ms"
      )
    end

    result
  end
end
