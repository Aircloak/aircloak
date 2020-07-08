defmodule Air.Repo.Migrations.ChangeExplorerAnalysisToTables do
  use Ecto.Migration
  alias Air.Repo

  def change do
    # This migration entails data loss as the meaning of the rows changes dramatically.
    # However, as this table works as somewhat of a Cache, this should be OK.
    Repo.delete_all(Air.Schemas.ExplorerAnalysis)

    alter(table(:explorer_analyses)) do
      remove(:column, :string)
      remove(:metrics, :text)
      add(:errors, {:array, :text}, null: false, default: [])
      add(:version, :string)
      add(:results, :map, null: false, default: %{"columns" => [], "sampleData" => []})
    end
  end
end
