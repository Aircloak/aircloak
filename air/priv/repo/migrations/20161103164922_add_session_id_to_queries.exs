defmodule Air.Repo.Migrations.AddSessionIdToQueries do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:session_id, :uuid)
    end
  end
end
