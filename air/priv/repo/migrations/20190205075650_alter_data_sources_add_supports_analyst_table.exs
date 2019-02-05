defmodule Air.Repo.Migrations.AlterDataSourcesAddSupportsAnalystTable do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:supports_analyst_tables, :boolean, default: false, null: false)
    end
  end
end
