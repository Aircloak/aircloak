defmodule Air.Repo.Migrations.AddIsolatedFailedToDataSources do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:isolated_failed, {:array, :string})
    end
  end
end
