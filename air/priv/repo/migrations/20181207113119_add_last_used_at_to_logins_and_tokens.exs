defmodule Air.Repo.Migrations.AddLastUsedAtToLoginsAndTokens do
  use Ecto.Migration

  def change do
    alter table(:logins) do
      add(:last_used_at, :naive_datetime)
    end

    alter table(:api_tokens) do
      add(:last_used_at, :naive_datetime)
    end
  end
end
