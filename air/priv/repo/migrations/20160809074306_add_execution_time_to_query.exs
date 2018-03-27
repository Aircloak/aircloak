defmodule Air.Repo.Migrations.AddExecutionTimeToQuery do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:execution_time, :integer)
    end
  end
end
