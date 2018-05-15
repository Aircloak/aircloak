defmodule Central.Repo.Migrations.DeleteQueriesTable do
  use Ecto.Migration

  def up do
    drop(table(:queries))
  end

  def down do
    create table(:queries) do
      add(:metrics, :map)
      add(:features, :map)
      add(:customer_id, references(:customers, on_delete: :delete_all))
      add(:aux, :map, default: fragment("'{}'::jsonb"))

      timestamps()
    end
  end
end
