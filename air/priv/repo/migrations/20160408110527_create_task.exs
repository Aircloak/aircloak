defmodule Air.Repo.Migrations.CreateTask do
  use Ecto.Migration

  def change do
    create table(:tasks, primary_key: false) do
      add :name, :string
      add :id, :uuid, primary_key: true
      add :query, :text
      add :user_id, references(:users, on_delete: :delete_all)

      timestamps()
    end
    create index(:tasks, [:user_id])

  end
end
