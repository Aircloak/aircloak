defmodule Air.Repo.Migrations.IndexQueriesOnQueryState do
  use Ecto.Migration

  def change do
    create index(:queries, [:query_state])
  end
end
