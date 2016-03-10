defmodule Air.Repo do
  @moduledoc false
  use Ecto.Repo, otp_app: :air

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
