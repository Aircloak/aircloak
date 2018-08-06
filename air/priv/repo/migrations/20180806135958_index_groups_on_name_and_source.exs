defmodule Air.Repo.Migrations.IndexGroupsOnNameAndSource do
  use Ecto.Migration

  import EctoEnum
  defenum(Source, :source, [:native, :ldap])

  def up do
    Source.create_type()

    alter table(:groups) do
      add(:source, :source, null: false, default: "native")
    end

    alter table(:users) do
      add(:source, :source, null: false, default: "native")
    end

    drop(unique_index(:groups, [:name]))
    create(unique_index(:groups, [:name, :source]))
  end

  def down do
    drop(unique_index(:groups, [:name, :source]))
    create(unique_index(:groups, [:name]))

    alter table(:groups) do
      remove(:source)
    end

    alter table(:users) do
      remove(:source)
    end

    Source.drop_type()
  end
end
