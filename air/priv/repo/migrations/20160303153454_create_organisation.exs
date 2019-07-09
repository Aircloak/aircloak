defmodule Air.Repo.Migrations.CreateOrganisation do
  use Ecto.Migration

  def change do
    create table(:organisations) do
      add(:name, :string)

      timestamps(type: :naive_datetime_usec)
    end
  end
end
