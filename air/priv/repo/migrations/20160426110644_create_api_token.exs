defmodule Air.Repo.Migrations.CreateApiToken do
  use Ecto.Migration

  def change do
    create table(:api_tokens) do
      add(:description, :string)
      add(:user_id, references(:users, on_delete: :delete_all))

      timestamps()
    end
  end
end
