defmodule Air.Repo.Migrations.AddBoundStatusToDataSources do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:bounds_computed_count, :integer)
      add(:bounds_failed, {:array, :string})
    end
  end
end
