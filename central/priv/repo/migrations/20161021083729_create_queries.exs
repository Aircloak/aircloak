defmodule Central.Repo.Migrations.CreateQueries do
  use Ecto.Migration

  def change do
    create table(:queries) do
      add :metrics, :map
      add :features, :map
      add :customer_id, references(:customers, on_delete: :delete_all)

      timestamps()
    end

    create index(:queries, [:customer_id])
  end
end
