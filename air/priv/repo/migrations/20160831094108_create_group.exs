defmodule Air.Repo.Migrations.CreateGroup do
  use Ecto.Migration

  def change do
    create table(:groups) do
      add :name, :string

      timestamps()
    end

    create unique_index(:groups, [:name])
  end
end
