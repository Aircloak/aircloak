defmodule Air.Repo.Migrations.CreateRoles do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add(:role_id, :integer, default: 0, null: false)
    end
  end
end
