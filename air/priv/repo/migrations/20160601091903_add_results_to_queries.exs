defmodule Air.Repo.Migrations.AddResultsToQueries do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:result, :text)
    end
  end
end
