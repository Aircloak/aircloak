defmodule Air.Repo.Migrations.ResultChunksOnDeleteCascade do
  use Ecto.Migration

  def up do
    execute("ALTER TABLE result_chunks DROP CONSTRAINT result_chunks_query_id_fkey")

    alter table(:result_chunks) do
      modify(:query_id, references(:queries, type: :uuid, on_delete: :delete_all))
    end
  end

  def down do
    execute("ALTER TABLE result_chunks DROP CONSTRAINT result_chunks_query_id_fkey")

    alter table(:result_chunks) do
      modify(:query_id, references(:queries, type: :uuid))
    end
  end
end
