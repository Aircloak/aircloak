defmodule Air.Repo.Migrations.AddAuCountToQuery do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add :au_count, :integer
    end
  end
end
