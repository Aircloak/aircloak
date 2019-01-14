defmodule Air.Repo.Migrations.AddRevokableTokens do
  use Ecto.Migration

  def up do
    Ecto.Migration.execute("CREATE TYPE revokable_token_type AS ENUM ('password_reset', 'session')")

    create table(:revokable_tokens, primary_key: false) do
      add(:id, :uuid, primary_key: true)
      add(:user_id, references(:users, on_delete: :delete_all), null: false)
      add(:type, :revokable_token_type, null: false)
      add(:payload, :binary, null: false)

      timestamps()
    end
  end

  def down do
    drop(table(:revokable_tokens))
    Ecto.Migration.execute("DROP TYPE revokable_token_type")
  end
end
