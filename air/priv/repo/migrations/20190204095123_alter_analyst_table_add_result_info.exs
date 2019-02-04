defmodule Air.Repo.Migrations.AlterAnalystTableAddResultInfo do
  use Ecto.Migration

  def up do
    execute("TRUNCATE TABLE analyst_tables")

    alter table(:analyst_tables) do
      add(:result_info, :map, default: fragment("'{}'::jsonb"))
      remove(:registration_info)
    end
  end

  def down do
    execute("TRUNCATE TABLE analyst_tables")

    alter table(:analyst_tables) do
      remove(:result_info)
      add(:registration_info, :text)
    end
  end
end
