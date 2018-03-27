defmodule Air.Repo.Migrations.AddPermanentFlagToTasks do
  use Ecto.Migration

  def change do
    alter table(:tasks) do
      add(:permanent, :boolean, default: false)
    end
  end
end
