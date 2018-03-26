defmodule Air.Repo.Migrations.CreateSettings do
  use Ecto.Migration

  def change do
    create table(:settings) do
      add(:query_retention_days, :integer)

      timestamps()
    end
  end
end
