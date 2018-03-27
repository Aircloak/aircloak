defmodule Air.Repo.Migrations.RecreateApiTokens do
  use Ecto.Migration

  def change do
    create table(:api_tokens, primary_key: false) do
      add(:id, :uuid, primary_key: true)
      add(:description, :string)
      add(:user_id, references(:users, on_delete: :delete_all))

      timestamps()
    end
  end
end
