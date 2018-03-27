defmodule Air.Repo.Migrations.AddCompletedToQueries do
  use Ecto.Migration

  def up do
    alter table(:queries) do
      add(:completed, :boolean, default: false)
    end

    # All past queries should be considered completed
    execute("UPDATE queries SET completed = true;")
  end

  def down do
    alter table(:queries) do
      remove(:completed)
    end
  end
end
