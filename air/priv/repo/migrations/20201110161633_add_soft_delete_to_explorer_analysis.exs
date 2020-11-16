defmodule Air.Repo.Migrations.AddSoftDeleteToExplorerAnalysis do
  use Ecto.Migration

  def change do
    alter table(:explorer_analyses) do
      add(:soft_delete, :boolean, default: false)
    end
  end
end
