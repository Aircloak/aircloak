defmodule Air.Repo.Migrations.ChangeQueryStatusToEnum do
  use Ecto.Migration

  import EctoEnum
  defenum(QueryStatus, :query_state, [:started, :completed])

  def up do
    __MODULE__.QueryStatus.create_type()

    alter table(:queries) do
      remove(:completed)
      add(:query_state, :query_state, default: "started")
    end

    execute("UPDATE queries SET query_state = 'completed'")
  end

  def down do
    alter table(:queries) do
      remove(:query_state)
      add(:completed, :boolean, default: false)
    end

    execute("UPDATE queries SET completed = true;")

    __MODULE__.QueryStatus.drop_type()
  end
end
