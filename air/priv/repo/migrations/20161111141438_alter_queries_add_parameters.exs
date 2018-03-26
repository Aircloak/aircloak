defmodule Air.Repo.Migrations.AlterQueriesAddParameters do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:parameters, :map, default: fragment("'{\"values\": []}'::jsonb"))
    end
  end
end
