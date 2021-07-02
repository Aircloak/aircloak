defmodule Air.Repo.Migrations.AddLevelToLogs do
  use Ecto.Migration
  alias Air.Schemas.Log

  def change do
    Log.Level.create_type()

    alter table("logs") do
      add(:level, Log.Level.type(), null: false, default: "info")
    end
  end
end
