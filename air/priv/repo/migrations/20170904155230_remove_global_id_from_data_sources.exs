defmodule Air.Repo.Migrations.RemoveGlobalIdFromDataSources do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      remove :global_id
    end
  end
end
