defmodule Air.Repo.Migrations.RenameUserEmailToLogin do
  use Ecto.Migration

  def change do
    rename(table(:users), :email, to: :login)
  end
end
