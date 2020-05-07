defmodule Air.Repo.Migrations.CreateExplorerAnalyses do
  use Ecto.Migration
  alias Air.Schemas.ExplorerAnalysis

  def up do
    ExplorerAnalysis.Status.create_type()

    create table(:explorer_analyses) do
      add(:table_name, :string)
      add(:column, :string)
      add(:status, ExplorerAnalysis.Status.type())
      add(:metrics, :text)
      add(:job_id, :string)
      add(:last_request, :naive_datetime)
      add(:data_source_id, references(:data_sources, on_delete: :delete_all))
    end

    create(index(:explorer_analyses, [:data_source_id]))
  end

  def down do
    drop(index(:explorer_analyses, [:data_source_id]))

    drop(table(:explorer_analyses))
    ExplorerAnalysis.Status.drop_type()
  end
end
