defmodule Central.Repo.Migrations.CascadeDeleteLicenses do
  use Ecto.Migration

  def up do
    execute("ALTER TABLE licenses DROP CONSTRAINT licenses_customer_id_fkey")

    alter table(:licenses) do
      modify(:customer_id, references(:customers, on_delete: :delete_all))
    end
  end

  def down do
    execute("ALTER TABLE licenses DROP CONSTRAINT licenses_customer_id_fkey")

    alter table(:licenses) do
      modify(:customer_id, references(:customers, on_delete: :null))
    end
  end
end
