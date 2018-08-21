defmodule Air.Repo.Migrations.AddTimeSpentToQueries do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:time_spent, :map, null: false, default: %{})
      add(:last_state_change_at, :naive_datetime)
    end
  end
end
