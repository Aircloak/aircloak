defmodule Central.Repo.Migrations.RemoveAirpcsAndCustomerExports do
  use Ecto.Migration

  def up do
    drop(table(:air_rpcs))
    drop(table(:customer_exports))
  end

  def down do
    create table(:air_rpcs, primary_key: false) do
      add(:id, :text, primary_key: true)

      timestamps(type: :naive_datetime_usec)
    end

    create table(:customer_exports) do
      add(:export_id, :integer, null: false)
      add(:customer_id, references(:customers, on_delete: :delete_all), null: false)
      timestamps(type: :naive_datetime_usec)
      add(:created_at, :naive_datetime_usec)
    end

    create(unique_index(:customer_exports, [:export_id, :customer_id]))
  end
end
