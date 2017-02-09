defmodule Central.Repo.Migrations.AddErrorToQuery do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add :error, :map
    end
  end
end
