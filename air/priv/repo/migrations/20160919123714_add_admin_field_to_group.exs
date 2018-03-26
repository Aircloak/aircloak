defmodule Air.Repo.Migrations.AddAdminFieldToGroup do
  use Ecto.Migration

  def change do
    alter table(:groups) do
      add(:admin, :boolean, default: false)
    end
  end
end
