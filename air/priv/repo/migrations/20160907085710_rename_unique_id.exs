defmodule Air.Repo.Migrations.RenameUniqueId do
  use Ecto.Migration

  def up do
    drop(unique_index(:data_sources, [:unique_id]))
    rename(table(:data_sources), :unique_id, to: :global_id)
    create(unique_index(:data_sources, [:global_id]))
  end

  def down do
    drop(unique_index(:data_sources, [:global_id]))
    rename(table(:data_sources), :global_id, to: :unique_id)
    create(unique_index(:data_sources, [:unique_id]))
  end
end
