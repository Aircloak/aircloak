defmodule Central.Repo.Migrations.CreateQueries do
  use Ecto.Migration

  def up do
    create table(:queries) do
      add(:metrics, :map)
      add(:features, :map)
      add(:customer_id, references(:customers, on_delete: :delete_all))

      timestamps(type: :naive_datetime_usec)
    end

    create(index(:queries, [:customer_id]))
  end

  def down, do: drop(table(:queries))
end
