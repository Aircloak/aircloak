defmodule Air.Repo.Migrations.AddSystemFlag do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add(:system, :boolean, default: false, null: false)
    end

    alter table(:groups) do
      add(:system, :boolean, default: false, null: false)
    end
  end
end
