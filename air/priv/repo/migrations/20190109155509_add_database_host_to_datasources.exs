defmodule Air.Repo.Migrations.AddDatabaseHostToDatasources do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:database_host, :string)
    end
  end
end
