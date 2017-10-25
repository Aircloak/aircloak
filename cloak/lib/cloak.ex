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
    configure_periodic_jobs()
    Supervisor.start_link(children(), strategy: :one_for_one, name: Cloak.Supervisor)
  end

  defp set_salt() do
    existing_env = Application.get_env(:cloak, :anonymizer)
    new_env = Keyword.put(existing_env, :salt, get_salt())
    Application.put_env(:cloak, :anonymizer, new_env)
  end

  # Conditional definition of top-level processes, since we don't want to run
  # all of them in the test environment.
  case Mix.env do
    :test -> defp children, do: common_processes()
    :dev -> defp children, do: common_processes() ++ system_processes()
    :prod -> defp children, do: common_processes() ++ system_processes()
  end

  defp common_processes, do:
    [
      Cloak.DataSource,
      Cloak.Query.Runner
    ]

  unless Mix.env in [:test] do
    # Processes which we don't want to start in the test environment
    defp system_processes, do:
      [
        Cloak.AirSocket,
        Cloak.MemoryReader,
      ]
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
