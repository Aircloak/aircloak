defmodule Central.Repo.Migrations.AddAuxFieldToQueries do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:aux, :map, default: fragment("'{}'::jsonb"))
    end
  end
end
