defmodule Air.Repo.Migrations.AddAccessToTokens do
  use Ecto.Migration

  def up() do
    Ecto.Migration.execute("CREATE TYPE api_token_access AS ENUM ('api', 'monitoring')")

    alter table(:api_tokens) do
      add(:access, :api_token_access, null: false, default: "api")
    end
  end

  def down() do
    alter table(:api_tokens) do
      remove(:access)
    end

    Ecto.Migration.execute("DROP TYPE IF EXISTS api_token_access")
  end
end
