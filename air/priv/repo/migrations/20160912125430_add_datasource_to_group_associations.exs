defmodule Air.Repo.Migrations.AddDataSourceToGroupAssociation do
  use Ecto.Migration

  def change do
    create table(:data_sources_groups, primary_key: false) do
      add(:group_id, references(:groups))
      add(:data_source_id, references(:data_sources))
    end
  end
end
