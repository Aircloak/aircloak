defmodule Air.Repo.Migrations.CreateResultChunks do
  use Ecto.Migration

  def change do
    create table(:result_chunks, primary_key: false) do
      add(:query_id, references(:queries, type: :uuid), null: false, primary_key: true)
      add(:offset, :integer, null: false, primary_key: true)
      add(:row_count, :integer, null: false)
      add(:encoded_data, :binary, null: false)
    end
  end
end
