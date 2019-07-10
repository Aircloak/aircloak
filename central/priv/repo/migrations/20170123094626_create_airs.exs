defmodule Central.Repo.Migrations.CreateAirs do
  use Ecto.Migration

  def change do
    create table(:airs) do
      add(:name, :string, null: false)
      add(:customer_id, references(:customers, on_delete: :delete_all), null: false)
      add(:status, :integer, null: false)
      timestamps(type: :naive_datetime_usec)
    end

    create(unique_index(:airs, [:name, :customer_id]))
  end
end
