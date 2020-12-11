defmodule Air.Repo.Migrations.MaterializeTotalTime do
  use Ecto.Migration

  def change do
    alter(table(:queries)) do
      add(:total_time, :integer,
        default: 0,
        null: false
      )
    end

    execute("UPDATE queries SET total_time = (SELECT SUM(CAST(value as int)) FROM jsonb_each(time_spent))", "SELECT 1")
  end
end
