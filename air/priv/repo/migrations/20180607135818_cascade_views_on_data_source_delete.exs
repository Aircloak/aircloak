defmodule Air.Repo.Migrations.CascadeViewsOnDataSourceDelete do
  use Ecto.Migration

  def up do
    execute("ALTER TABLE views DROP CONSTRAINT views_data_source_id_fkey")

    alter table(:views) do
      modify(:data_source_id, references(:data_sources, on_delete: :delete_all), null: false)
    end
  end

  def down do
    execute("ALTER TABLE views DROP CONSTRAINT views_data_source_id_fkey")

    alter table(:views) do
      modify(:data_source_id, references(:data_sources), null: false)
    end
  end
end
