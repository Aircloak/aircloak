defmodule Air.Repo.Migrations.CreateViews do
  use Ecto.Migration

  def change do
    create table(:views) do
      add(:user_id, references(:users), null: false)
      add(:data_source_id, references(:data_sources), null: false)
      add(:name, :text, null: false)
      add(:sql, :text, null: false)
    end

    create(unique_index(:views, [:user_id, :data_source_id, :name]))
  end
end
