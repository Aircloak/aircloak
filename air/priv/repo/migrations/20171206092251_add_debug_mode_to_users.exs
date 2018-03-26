defmodule Air.Repo.Migrations.AddDebugModeToUsers do
  use Ecto.Migration

  def change do
    alter table("users") do
      add(:debug_mode_enabled, :boolean, default: false)
    end
  end
end
