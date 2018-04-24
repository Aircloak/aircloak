defmodule Air.Repo do
  @moduledoc false
  use Ecto.Repo, otp_app: :air
  use Scrivener, page_size: 20
  use Aircloak.ChildSpec.Supervisor

  require Aircloak.DeployConfig

  require Logger

  @doc """
  Reads database settings and merges them into the existing repo
  configuration as specified in `config.exs`.

  This allows us to change database settings for different deployments without needing to bake them into the
  release.
  """
  def configure do
    conn_params = [
      hostname: db_setting!("host"),
      port: db_setting("port", 5432),
      ssl: db_setting("ssl", false),
      username: db_setting!("user"),
      password: db_setting("password", ""),
      database: db_setting!("name")
    ]

    Logger.info("connecting to database #{inspect(Keyword.delete(conn_params, :password))}")
    Air.Utils.update_app_env(:air, Air.Repo, &Keyword.merge(&1, conn_params))
  end

  defp db_setting!(name), do: Map.fetch!(Aircloak.DeployConfig.fetch!("database"), name)

  defp db_setting(name, default) do
    case Map.fetch(Aircloak.DeployConfig.fetch!("database"), name) do
      {:ok, value} ->
        value

      :error ->
        Logger.info("using default value for database `#{name}` parameter")
        default
    end
  end

  defmodule Migrator do
    @moduledoc false
    use GenServer, restart: :transient, start: {__MODULE__, :start_link, []}

    # Note: we're using GenServer (instead of e.g. `Task`) to ensure that the rest
    # of the system waits until the database is migrated.

    def start_link, do: GenServer.start_link(__MODULE__, nil)

    def init(_) do
      migrate_with_retry!()
      Logger.info("database migrated")

      # stops the server without crashing the supervisor
      :ignore
    end

    defp migrate_with_retry!(retries \\ 12)

    defp migrate_with_retry!(0), do: migrate!()

    defp migrate_with_retry!(retries) do
      try do
        migrate!()
      rescue
        DBConnection.ConnectionError ->
          Process.sleep(:timer.seconds(5))
          migrate_with_retry!(retries - 1)
      end
    end

    defp migrate!() do
      Ecto.Migrator.run(
        Air.Repo,
        Application.app_dir(:air, "priv/repo/migrations"),
        :up,
        all: true
      )
    end
  end
end
