defmodule Air.Repo.Migrations.CreateResult do
  use Ecto.Migration

  def change do
    create table(:results) do
      add(:buckets, :text)
      add(:exceptions, :text)
      add(:post_processed, :text)
      add(:task_id, references(:tasks, on_delete: :delete_all, type: :uuid))

      timestamps(type: :naive_datetime_usec)
    end

    create(index(:results, [:task_id]))
  end
end
