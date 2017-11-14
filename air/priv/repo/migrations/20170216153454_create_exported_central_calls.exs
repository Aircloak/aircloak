defmodule Air.Repo.Migrations.CreateExportedCentralCalls do
  use Ecto.Migration

  def change do
    create table(:exported_central_calls) do
      add :payload, :binary

      timestamps()
    end
  end
end
