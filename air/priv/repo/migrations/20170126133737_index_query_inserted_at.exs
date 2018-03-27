defmodule Air.Repo.Migrations.IndexQueryInsertedAt do
  use Ecto.Migration

  def change do
    create(index(:queries, [:inserted_at]))
  end
end
