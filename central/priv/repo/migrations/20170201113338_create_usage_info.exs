defmodule Central.Repo.Migrations.CreateUsageInfo do
  use Ecto.Migration

  def change do
    create table(:usage_info) do
      add(:air_utc_time, :naive_datetime, null: false)
      add(:data, :map, null: false)
      add(:customer_id, references(:customers), null: false)
      add(:air_id, references(:airs), null: false)
      timestamps()
    end
  end
end
