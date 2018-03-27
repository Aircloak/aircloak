defmodule Air.Repo.Migrations.RemoveRows do
  use Ecto.Migration

  def up do
    alter table(:queries) do
      remove(:rows)
    end
  end

  def down do
    alter table(:queries) do
      add(:rows, :binary)
    end
  end
end
