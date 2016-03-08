defmodule Air.EtcdConfig do
  @moduledoc """
  Helpers for accessing etcd configuration settings at compile time. This is meant
  to be used only in config.exs and associated scripts, to generate proper application
  configuration.
  """

  @mix_env Mix.env

  @doc "Returns the tcp port for the given air service"
  def tcp_port(service) do
    bash_exec(". ../config/config.sh && echo $(get_tcp_port #{@mix_env} #{service})")
    |> String.to_integer
  end

  @doc "Returns etcd value for the given key"
  def etcd_get(key) do
    bash_exec(". ../etcd/etcd_lib.sh && init_env #{@mix_env} && etcd_get #{key}")
  end

  defp bash_exec(cmd) do
    {result, 0} = System.cmd("bash", ["-c", cmd])
    String.rstrip(result)
  end
end
