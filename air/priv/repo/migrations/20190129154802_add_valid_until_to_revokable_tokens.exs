defmodule Air.Repo.Migrations.AddValidUntilToRevokableTokens do
  use Ecto.Migration

  def up do
    alter table(:revokable_tokens) do
      add(:valid_until, :naive_datetime)
    end

    execute("UPDATE revokable_tokens SET valid_until = '1970-01-01 12:00:00'")

    alter table(:revokable_tokens) do
      modify(:valid_until, :naive_datetime, null: false)
    end

    create(index(:revokable_tokens, [:valid_until]))
  end

  def down do
    drop(index(:revokable_tokens, [:valid_until]))

    alter table(:revokable_tokens) do
      remove(:valid_until)
    end
  end
end
