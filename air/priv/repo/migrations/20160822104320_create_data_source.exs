defmodule Air.Repo.Migrations.CreateDataSource do
  use Ecto.Migration

  def change do
    create table(:data_sources) do
      # This is used in addition to, rather than instead of, the generic
      # numerical ID column, as it simplifies working with Ecto.
      add :unique_id, :string

      add :name, :string
      add :tables, :text

      timestamps()
    end

    create unique_index(:data_sources, [:unique_id])
  end
end
