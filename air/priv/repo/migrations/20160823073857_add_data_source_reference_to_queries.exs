defmodule Air.Repo.Migrations.AddDataSourceReferenceToQueries do
  use Ecto.Migration

  def up do
    alter table(:queries) do
      remove(:cloak_id)
      remove(:data_source)
      add(:data_source_id, references(:data_sources, on_delete: :delete_all))
    end
  end

  def down do
    alter table(:queries) do
      add(:cloak_id, :string)
      add(:data_source, :string)
      remove(:data_source_id)
    end
  end
end
