defmodule Central.Repo.Migrations.AlterRpcsRemoveResult do
  use Ecto.Migration

  def up() do
    alter table(:air_rpcs) do
      remove(:result)
    end
  end

  def down() do
    alter table(:air_rpcs) do
      add(:result, :binary)
    end
  end
end
