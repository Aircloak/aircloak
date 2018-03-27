defmodule Air.Repo.Migrations.DropResultsTable do
  use Ecto.Migration

  def up do
    drop(table(:results))
  end

  def down do
    create table(:results) do
      add(:buckets, :text)
      add(:exceptions, :text)
      add(:post_processed, :text)
      add(:query_id, references(:queries, on_delete: :delete_all, type: :uuid))

      timestamps()
    end

    create(index(:results, [:query_id]))
  end
end
