defmodule Air.Repo.Migrations.AlterDataSourceAddColumnCounts do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:columns_count, :integer)
      add(:isolated_computed_count, :integer)
    end
  end
end
