defmodule Air.Repo.Migrations.CreateAnalystTables do
  use Ecto.Migration

  def change do
    create table(:analyst_tables) do
      add(:user_id, references(:users), null: false)
      add(:data_source_id, references(:data_sources), null: false)
      add(:name, :text, null: false)
      add(:sql, :text, null: false)
      add(:registration_info, :text)
    end

    create(unique_index(:analyst_tables, [:user_id, :data_source_id, :name]))
  end
end
