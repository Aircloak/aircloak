defmodule Air.Repo.Migrations.IndexGroupsOnNameAndSource do
  use Ecto.Migration

  import EctoEnum
  defenum(GroupSource, :group_source, [:native, :ldap])

  def up do
    GroupSource.create_type()

    alter table(:groups) do
      add(:source, :group_source, null: false, default: "native")
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

    GroupSource.drop_type()
  end
end
