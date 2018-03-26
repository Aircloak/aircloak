defmodule Air.Repo.Migrations.RemoveRoleIdFromUser do
  use Ecto.Migration

  def up do
    alter table(:users) do
      remove(:role_id)
    end
  end

  def down do
    alter table(:users) do
      add(:role_id, :integer)
    end
  end
end
