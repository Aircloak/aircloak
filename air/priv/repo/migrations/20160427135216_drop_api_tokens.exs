defmodule Air.Repo.Migrations.DropApiTokens do
  use Ecto.Migration

  def up do
    drop table(:api_tokens)
  end

  def down do
    create table(:api_tokens) do
      add :description, :string
      add :user_id, references(:users, on_delete: :delete_all)

      timestamps()
    end
  end
end
