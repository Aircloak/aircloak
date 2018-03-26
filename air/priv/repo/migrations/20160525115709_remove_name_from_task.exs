defmodule Air.Repo.Migrations.RemoveNameFromTask do
  use Ecto.Migration

  def up do
    alter table(:tasks) do
      remove(:name)
    end
  end

  def down do
    alter table(:tasks) do
      add(:name, :string)
    end
  end
end
