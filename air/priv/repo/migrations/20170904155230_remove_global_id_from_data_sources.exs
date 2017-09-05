defmodule Air.Repo.Migrations.RemoveGlobalIdFromDataSources do
  use Ecto.Migration

  def up do
    alter table(:data_sources) do
      remove :global_id
    end
  end

  def down do
    alter table(:data_sources) do
      add :global_id, :string
    end
    create unique_index(:data_sources, [:global_id])
  end
end
