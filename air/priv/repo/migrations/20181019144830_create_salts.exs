defmodule Air.Repo.Migrations.CreateSalts do
  use Ecto.Migration

  def change do
    create table(:salts) do
      add(:name, :string, null: false)
      add(:value, :string, null: false)
      timestamps(type: :naive_datetime_usec)
    end

    create(unique_index(:salts, [:name]))
  end
end
