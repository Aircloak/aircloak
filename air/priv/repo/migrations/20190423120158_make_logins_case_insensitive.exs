defmodule Air.Repo.Migrations.MakeLoginsCaseInsensitive do
  use Ecto.Migration

  def up do
    drop(unique_index(:logins, [:login]))
    execute("CREATE UNIQUE INDEX logins_login_index ON logins (lower(login))")
  end

  def down do
    drop(unique_index(:logins, [:login]))
    create(unique_index(:logins, [:login]))
  end
end
