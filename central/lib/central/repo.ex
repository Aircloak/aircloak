defmodule Central.Repo do
  @moduledoc false
  use Ecto.Repo, otp_app: :central
  use Aircloak.ChildSpec.Supervisor
  require Aircloak.DeployConfig

  # Need to disable due to error in old Ecto. Should be revised once we upgrade Ecto to 2.0
  @dialyzer :no_undefined_callbacks

  require Logger

  @doc """
  Reads database settings and merges them into the existing repo
  configuration as specified in `config.exs`.

  This allows us to change database settings for different deployments without needing to bake them into the
  release.
  """
  def configure do
    conn_params = [
      hostname: db_setting("host"),
      port: db_setting("port"),
      ssl: db_setting("ssl"),
      username: db_setting("user"),
      password: db_setting("password"),
      database: db_setting("name")
    ]

    Logger.info("connecting to database #{inspect(Keyword.delete(conn_params, :password))}")
    Central.Utils.update_app_env(:central, Central.Repo, &Keyword.merge(&1, conn_params))
  end

  defp db_setting(name), do: Map.fetch!(Aircloak.DeployConfig.fetch!("database"), name)

  defmodule Migrator do
    @moduledoc false
    use GenServer, start: {__MODULE__, :start_link, []}, restart: :transient

    # Note: we're using GenServer (instead of e.g. `Task`) to ensure that the rest
    # of the system waits until the database is migrated.

    def start_link, do: GenServer.start_link(__MODULE__, nil)

    def init(_) do
      Ecto.Migrator.run(
        Central.Repo,
        Application.app_dir(:central, "priv/repo/migrations"),
        :up,
        all: true
      )
      Logger.info("database migrated")
      :ignore # stops the server without crashing the supervisor
    end
  end
end
