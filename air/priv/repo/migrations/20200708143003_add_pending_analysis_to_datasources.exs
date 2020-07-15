defmodule Air.Repo.Migrations.SimplifyDataSource do
  use Ecto.Migration

  def up do
    alter table(:data_sources) do
      remove(:columns_count)
      remove(:isolated_computed_count)
      remove(:shadow_tables_computed_count)
      remove(:bounds_computed_count)
      add(:pending_analysis, :boolean, default: false, null: false)
    end
  end

  def down do
    alter table(:data_sources) do
      add(:columns_count, :integer)
      add(:isolated_computed_count, :integer)
      add(:shadow_tables_computed_count, :integer)
      add(:bounds_computed_count, :integer)
      remove(:pending_analysis)
    end
  end
end
