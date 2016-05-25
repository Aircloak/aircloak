defmodule Air.Repo.Migrations.RemovePermanentFromTasks do
  use Ecto.Migration

  def change do
    alter table(:tasks) do
      remove :permanent
    end
  end
end
