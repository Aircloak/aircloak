defmodule Air.Repo.Migrations.CreateExportedCentralCalls do
  use Ecto.Migration

  def change do
    create table(:exported_central_calls) do
      add(:payload, :binary)

      timestamps(type: :naive_datetime_usec)
    end
  end
end
