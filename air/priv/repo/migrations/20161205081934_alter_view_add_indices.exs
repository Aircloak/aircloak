defmodule Air.Repo.Migrations.AlterViewAddIndices do
  use Ecto.Migration

  def change do
    create(index(:views, [:user_id]))
    create(index(:views, [:data_source_id]))
  end
end
