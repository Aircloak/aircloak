defmodule Air.Repo.Migrations.RenameUserLoginIndex do
  use Ecto.Migration

  def up do
    drop(unique_index(:users, :login, name: :users_email_index))
    create(unique_index(:users, :login))
  end

  def down do
    drop(unique_index(:users, :login))
    create(unique_index(:users, :login, name: :users_email_index))
  end
end
