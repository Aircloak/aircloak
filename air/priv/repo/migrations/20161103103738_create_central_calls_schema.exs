defmodule Air.Repo.Migrations.CreateCentralCallsSchema do
  use Ecto.Migration

  def change do
    create table(:central_calls) do
      add :event, :string
      add :payload, :map, default: fragment("'{}'::jsonb")

      timestamps()
    end
  end
end
