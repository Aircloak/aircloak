defmodule Central.Repo.Migrations.DropAirCloakAndUsageInfoTables do
  use Ecto.Migration

  def up do
    drop table(:cloaks)
    drop table(:usage_info)
    drop table(:airs)
  end

  def down do
    create table(:airs) do
      add :name, :string, null: false
      add :customer_id, references(:customers, on_delete: :delete_all), null: false
      add :status, :integer, null: false
      add :version, :string
      timestamps()
    end
    create unique_index(:airs, [:name, :customer_id])

    create table(:usage_info) do
      add :air_utc_time, :naive_datetime, null: false
      add :data, :map, null: false
      add :customer_id, references(:customers), null: false
      add :air_id, references(:airs), null: false
      timestamps()
    end

    create table(:cloaks) do
      add :name, :string, null: false
      add :air_id, references(:airs, on_delete: :delete_all), null: false
      add :status, :integer, null: false
      add :version, :string
      add :data_source_names, {:array, :string}, null: false, default: []
      timestamps()
    end

    create unique_index(:cloaks, [:name, :air_id])
  end
end
