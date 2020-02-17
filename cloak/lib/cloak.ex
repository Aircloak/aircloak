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

    with {:ok, lcf_buckets_aggregation_limit} <- Aircloak.DeployConfig.fetch("lcf_buckets_aggregation_limit"),
         do: Application.put_env(:cloak, :lcf_buckets_aggregation_limit, lcf_buckets_aggregation_limit)

    case Aircloak.DeployConfig.fetch("connection_keep_time") do
      {:ok, minutes} -> Application.put_env(:cloak, :connection_keep_time, :timer.minutes(minutes))
      _ -> Application.put_env(:cloak, :connection_keep_time, Cloak.DataSource.Driver.connection_keep_time())
    end

    with {:ok, true} <- Aircloak.DeployConfig.fetch("enable_case_support"),
         do: Application.put_env(:cloak, :enable_case_support, true)

    with {:ok, _} = result <- Supervisor.start_link(children(), strategy: :one_for_one, name: Cloak.Supervisor) do
      log_startup()
      result
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp children() do
    import Aircloak, only: [in_env: 1]

    [
      Cloak.MemoryUsage,
      Cloak.AnalystTable,
      Cloak.DataSource,
      Cloak.Query.Runner,
      in_env(test: nil, else: Cloak.AirSocket),
      in_env(test: nil, else: Cloak.MemoryReader)
    ]
    |> Enum.reject(&is_nil/1)
  end

  defp log_startup() do
    {:ok, air_site} = Aircloak.DeployConfig.fetch("air_site")
    version = Aircloak.Version.for_app(:cloak)
    Logger.info("Insights Cloak version #{version} started [air_site: #{air_site}]")
  end

  defp set_salt() do
    existing_env = Application.get_env(:cloak, :anonymizer)
    new_env = Keyword.put(existing_env, :salt, get_salt())
    Application.put_env(:cloak, :anonymizer, new_env)
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
