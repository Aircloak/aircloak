defmodule Central.Repo.Migrations.CreateCustomerExports do
  use Ecto.Migration

  def change do
    create table(:customer_exports) do
      add(:export_id, :integer, null: false)
      add(:customer_id, references(:customers, on_delete: :delete_all), null: false)
      timestamps()
    end

    create(unique_index(:customer_exports, [:export_id, :customer_id]))
  end
end
