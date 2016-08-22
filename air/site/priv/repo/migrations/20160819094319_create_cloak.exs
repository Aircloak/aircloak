defmodule Air.Repo.Migrations.CreateCloak do
  use Ecto.Migration

  def change do
    create table(:cloaks) do
      add :name, :text
      add :state_int, :integer

      timestamps
    end

    create index(:cloaks, [:name])
  end
end
