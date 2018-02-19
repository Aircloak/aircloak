defmodule Cloak do
  @moduledoc false
  use Application
  require Aircloak.DeployConfig
  require Logger

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Cloak.LoggerTranslator.install()
    set_salt()
    if Aircloak.DeployConfig.fetch("debug") === {:ok, true} do Logger.configure(level: :debug) end
    with {:ok, concurrency} <- Aircloak.DeployConfig.fetch("concurrency"), do:
      Application.put_env(:cloak, :concurrency, concurrency)
    with {:ok, aes_key} <- Aircloak.DeployConfig.fetch("aes_key"), do:
      Application.put_env(:cloak, :aes_key, aes_key)
    configure_periodic_jobs()
    Supervisor.start_link(children(), strategy: :one_for_one, name: Cloak.Supervisor)
  end

  defp set_salt() do
    existing_env = Application.get_env(:cloak, :anonymizer)
    new_env = Keyword.put(existing_env, :salt, get_salt())
    Application.put_env(:cloak, :anonymizer, new_env)
  end

  defp children, do: common_processes() ++ system_processes()

  defp common_processes(), do: [Cloak.DataSource, Cloak.Query.Runner, Cloak.Performance]

  if Mix.env == :test do
    defp system_processes(), do: []
  else
    defp system_processes, do: [Cloak.AirSocket, Cloak.MemoryReader]
  end

  defp get_salt() do
    case Aircloak.DeployConfig.fetch("salt") do
      :error ->
        raise("Please specify a salt in the cloak configuration file (config.json). " <>
          "The salt is a requirement for strong anonymization.")
      {:ok, value} -> value
    end
  end

  if Mix.env == :test do
    defp configure_periodic_jobs(), do: :ok
  else
    defp configure_periodic_jobs() do
      [{"*/5 * * * *", {Cloak.DataSource.SerializingUpdater, :run_liveness_check}}]
      |> Enum.each(fn({schedule, job}) -> Quantum.add_job(schedule, job) end)
    end
  end
end
