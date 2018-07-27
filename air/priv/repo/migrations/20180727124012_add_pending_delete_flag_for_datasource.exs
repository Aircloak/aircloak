defmodule Air.Repo.Migrations.AddPendingDeleteFlagForDatasource do
  use Ecto.Migration

  def change do
    alter table(:data_source) do
      add(:pending_delete, :boolean, default: false)
    end
  end
end
