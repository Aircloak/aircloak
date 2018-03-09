defmodule Central.Repo.Migrations.CreateLicenses do
  use Ecto.Migration

  def change do
    create table(:licenses) do
      add :customer_id, references(:customers)
      add :name, :string

      timestamps()
    end
  end
end
