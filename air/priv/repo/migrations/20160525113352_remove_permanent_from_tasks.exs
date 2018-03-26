defmodule Air.Repo.Migrations.RemovePermanentFromTasks do
  use Ecto.Migration

  def up do
    alter table(:tasks) do
      remove(:permanent)
    end
  end

  def down do
    alter table(:tasks) do
      add(:permanent, :boolean, default: false)
    end
  end
end
