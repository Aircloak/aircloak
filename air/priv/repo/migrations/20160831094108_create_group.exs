defmodule Air.Repo.Migrations.CreateGroup do
  use Ecto.Migration

  def change do
    create table(:groups) do
      add(:name, :string)

      timestamps(type: :naive_datetime_usec)
    end

    create(unique_index(:groups, [:name]))
  end
end
