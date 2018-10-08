defmodule Air.Repo.Migrations.AddShadowTableStatesToDataSources do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:shadow_tables_computed_count, :integer)
      add(:shadow_tables_failed, {:array, :string})
    end
  end
end
