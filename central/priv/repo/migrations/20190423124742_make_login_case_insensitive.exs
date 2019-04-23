defmodule Central.Repo.Migrations.MakeLoginCaseInsensitive do
  use Ecto.Migration

  def up do
    drop(unique_index(:users, [:email]))
    execute("CREATE UNIQUE INDEX users_email_index ON users (lower(email))")
  end

  def down do
    drop(unique_index(:users, [:email]))
    create(unique_index(:users, [:email]))
  end
end
