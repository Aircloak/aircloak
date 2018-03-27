defmodule Air.Repo.Migrations.AddIngestingAndPostProcessingQueryStates do
  use Ecto.Migration

  import EctoEnum

  defenum(OldQueryState, :query_state, [
    :started,
    :parsing,
    :compiling,
    :awaiting_data,
    :processing,
    :completed,
    :error,
    :cancelled
  ])

  defenum(QueryState, :query_state, [
    :started,
    :parsing,
    :compiling,
    :awaiting_data,
    :ingesting_data,
    :processing,
    :post_processing,
    :completed,
    :error,
    :cancelled
  ])

  def up do
    alter table(:queries) do
      remove(:query_state)
    end

    OldQueryState.drop_type()
    QueryState.create_type()

    alter table(:queries) do
      add(:query_state, :query_state, default: "started")
    end

    create(index(:queries, [:query_state]))

    execute("UPDATE queries SET query_state = 'completed'")
  end

  def down do
    alter table(:queries) do
      remove(:query_state)
    end

    QueryState.drop_type()
    OldQueryState.create_type()

    alter table(:queries) do
      add(:query_state, :query_state, default: "started")
    end

    execute("UPDATE queries SET query_state = 'completed'")
  end
end
