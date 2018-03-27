defmodule Air.Repo.Migrations.AlterChunks do
  use Ecto.Migration

  def change do
    Air.Repo.delete_all("result_chunks")

    rename(table(:result_chunks), :offset, to: :index)

    alter table(:result_chunks) do
      remove(:row_count)
    end
  end

  def down do
    Air.Repo.delete_all("result_chunks")
    rename(table(:result_chunks), :index, to: :offset)

    alter table(:result_chunks) do
      add(:row_count, :integer, null: false)
    end
  end
end
