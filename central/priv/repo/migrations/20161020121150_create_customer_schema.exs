defmodule Central.Repo.Migrations.CreateCustomerSchema do
  use Ecto.Migration

  def change do
    create table(:customers) do
      add :name, :string
      timestamps()
    end

    create unique_index(:customers, [:name])
  end
end
