defmodule Air.Repo.Migrations.AddActiveToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add(:enabled, :boolean, default: true)
    end

    create(index(:users, [:enabled]))
  end
end
