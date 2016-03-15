defmodule Air.Repo.Migrations.AddUniqueNameToOrganisation do
  use Ecto.Migration

  def change do
    create unique_index(:organisations, [:name])
  end
end
