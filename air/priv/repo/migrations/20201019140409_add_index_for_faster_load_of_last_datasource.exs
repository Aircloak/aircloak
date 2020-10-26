defmodule Air.Repo.Migrations.AddIndexForFasterLoadOfLastDataSource do
  use Ecto.Migration

  def change do
    create(index(:queries, [:user_id, :context]))
  end
end
