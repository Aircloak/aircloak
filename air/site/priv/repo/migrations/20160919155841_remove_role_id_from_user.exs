defmodule Air.Repo.Migrations.RemoveRoleIdFromUser do
  use Ecto.Migration

  def change do
    alter table(:users) do
      remove :role_id
    end
  end
end
