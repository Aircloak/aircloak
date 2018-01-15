defmodule Central.Repo.Migrations.CreateUser do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string
      add :hashed_password, :string
      add :name, :string

      timestamps()
    end

    # We want the uniqueness constraint to be
    # enforced by the database, rather than
    # through a query like it is normally done
    # in for example rails.
    create unique_index(:users, [:email])
  end
end
