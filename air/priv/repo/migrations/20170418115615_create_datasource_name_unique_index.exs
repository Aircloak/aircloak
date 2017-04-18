defmodule Air.Repo.Migrations.CreateDatasourceNameUniqueIndex do
  use Ecto.Migration

  def change do
    create unique_index(:data_sources, [:name])
  end
end
