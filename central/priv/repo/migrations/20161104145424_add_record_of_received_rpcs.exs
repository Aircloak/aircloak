defmodule Central.Repo.Migrations.AddRecordOfReceivedRpcs do
  use Ecto.Migration

  def change do
    create table(:air_rpcs, primary_key: false) do
      add :id, :string, primary_key: true
      add :result, :binary

      timestamps()
    end
  end
end
