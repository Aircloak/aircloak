defmodule Aircloak do
  @moduledoc false
  use Application

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
end
