defmodule Air.EnvSettings do
  @moduledoc """
  Helpers for accessing configuration settings which are defined through OS
  environment variables. This is meant to be used only in config.exs and associated
  scripts, to generate proper application configuration.
  """

  @mix_env Mix.env

  @doc "Returns the tcp port for the given air service"
  def tcp_port(service) do
    bash_exec(". ../config/config.sh && echo $(get_tcp_port #{@mix_env} #{service})")
    |> String.to_integer
  end

  defp bash_exec(cmd) do
    {result, 0} = System.cmd("bash", ["-c", cmd])
    String.rstrip(result)
  end
end
