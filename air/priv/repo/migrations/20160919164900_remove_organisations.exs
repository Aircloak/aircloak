defmodule Air.Repo.Migrations.RemoveOrganisations do
  use Ecto.Migration

  def up do
    alter table(:users) do
      remove(:organisation_id)
    end

    drop(table(:organisations))
  end

  def down do
    create table(:organisations) do
      add(:name, :string)
      timestamps()
    end

    alter table(:users) do
      add(:organisation_id, :integer)
    end
  end
end
