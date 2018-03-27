defmodule Air.Repo.Migrations.AddExtractedFeaturesToQueries do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:features, :map)
    end
  end
end
