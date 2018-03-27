defmodule Air.Repo.Migrations.RenameTasksTableToQueries do
  use Ecto.Migration

  def up do
    rename(table(:tasks), to: table(:queries))
    drop_if_exists(index(:results, [:task_id]))
    # We are doing a destructive operation here. Instead of renaming it.
    # The reason being that we are in an early transition phase, and
    # this really is a temporary change until results are inlined into
    # the queries themselves.
    execute("DELETE FROM results")

    alter table(:results) do
      remove(:task_id)
      add(:query_id, references(:queries, on_delete: :delete_all, type: :uuid))
    end

    create(index(:results, [:query_id]))
  end

  def down do
    rename(table(:queries), to: table(:tasks))
    drop_if_exists(index(:results, [:query_id]))
    # We are doing a destructive operation here. Instead of renaming it.
    # The reason being that we are in an early transition phase, and
    # this really is a temporary change until results are inlined into
    # the queries themselves.
    execute("DELETE FROM results")

    alter table(:results) do
      remove(:query_id)
      add(:task_id, references(:tasks, on_delete: :delete_all, type: :uuid))
    end

    create(index(:results, [:task_id]))
  end
end
