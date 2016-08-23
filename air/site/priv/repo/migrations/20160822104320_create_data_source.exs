defmodule Air.Repo.Migrations.CreateDataSource do
  use Ecto.Migration

  def change do
    create table(:data_sources) do
      add :name, :string
      add :tables, :text
      add :cloak_id, references(:cloaks, on_delete: :delete_all)

      timestamps
    end

    create index(:data_sources, [:cloak_id])
  end
end
