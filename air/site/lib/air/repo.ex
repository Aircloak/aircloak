defmodule Air.Repo do
  @moduledoc false
  use Ecto.Repo, otp_app: :air

  # Need to disable due to error in old Ecto. Should be revised once we upgrade Ecto to 2.0
  @dialyzer :no_undefined_callbacks

  @doc """
  Reads database settings from etcd and merges them into the existing repo
  configuration as specified in `config.exs`.

  This allows us to change database settings via etcd without needing to bake them into the
  release.
  """
  def configure do
    Air.Utils.update_app_env(
      :air, Air.Repo,
      &Keyword.merge(&1,
        hostname: :air_etcd.get("/settings/air/db/host"),
        port: String.to_integer(:air_etcd.get("/settings/air/db/port")),
        ssl: String.to_existing_atom(:air_etcd.get("/settings/air/db/ssl")),
        database: :air_etcd.get("/settings/air/db/insights_database"),
        username: :air_etcd.get("/settings/air/db/username"),
        password: :air_etcd.get("/settings/air/db/password"),
      )
    )
  end

  defmodule Migrator do
    @moduledoc false
    use GenServer

    # Note: we're using GenServer (instead of e.g. `Task`) to ensure that the rest
    # of the system waits until the database is migrated.

    def start_link, do: GenServer.start_link(__MODULE__, nil)

    def init(_) do
      Ecto.Migrator.run(
        Air.Repo,
        Application.app_dir(:air, "priv/repo/migrations"),
        :up,
        all: true
      )
      Logger.info("database migrated")
      :ignore # stops the server without crashing the supervisor
    end
  end
end
