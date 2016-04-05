defmodule Air.Repo.Migrations.CreateAdminGroup do
  use Ecto.Migration
  require Logger

  def up do
    %{command: :select, rows: [[count]]} =
      Ecto.Adapters.SQL.query!(Air.Repo,"SELECT count(*) FROM organisations WHERE name=$1", ["administrators"])

    if count == 0 do
      %{num_rows: 1} = Ecto.Adapters.SQL.query!(
          Air.Repo,
          "
            INSERT INTO organisations(name, inserted_at, updated_at)
            VALUES($1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
          ",
          ["administrators"]
        )
    else
      Logger.info("Administrators group already exists")
    end
  end

  def down do
  end
end
