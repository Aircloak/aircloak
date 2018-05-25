defmodule Cloak do
  @moduledoc false
  use Application
  require Aircloak.DeployConfig
  require Logger

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Aircloak.DeployConfig.validate!(:cloak)

    Cloak.LoggerTranslator.install()
    set_salt()

    if Aircloak.DeployConfig.fetch("debug") === {:ok, true} do
      Logger.configure(level: :debug)
    end

    with {:ok, concurrency} <- Aircloak.DeployConfig.fetch("concurrency"),
         do: Application.put_env(:cloak, :concurrency, concurrency)

    with {:ok, aes_key} <- Aircloak.DeployConfig.fetch("aes_key"), do: Application.put_env(:cloak, :aes_key, aes_key)

    Cloak.DataSource.RODBC.Driver.init!()
    Supervisor.start_link(children(), strategy: :one_for_one, name: Cloak.Supervisor)
  end

  defp set_salt() do
    existing_env = Application.get_env(:cloak, :anonymizer)
    new_env = Keyword.put(existing_env, :salt, get_salt())
    Application.put_env(:cloak, :anonymizer, new_env)
  end

  # Conditional definition of top-level processes, since we don't want to run
  # all of them in the test environment.
  case Mix.env() do
    :test -> defp children, do: common_processes()
    :dev -> defp children, do: common_processes() ++ system_processes()
    :prod -> defp children, do: common_processes() ++ system_processes()
  end

  defp common_processes,
    do: [
      Cloak.DataSource,
      Cloak.Query.Runner,
      Cloak.DataSource.Isolators
    ]

  unless Mix.env() in [:test] do
    # Processes which we don't want to start in the test environment
    defp system_processes,
      do: [
        Cloak.AirSocket,
        Cloak.MemoryReader,
        Cloak.Scheduler
      ]
  end

  defp get_salt() do
    case Aircloak.DeployConfig.fetch("salt") do
      :error ->
        raise(
          "Please specify a salt in the cloak configuration file (config.json). " <>
            "The salt is a requirement for strong anonymization."
        )

      {:ok, value} ->
        value
    end
  end
end
