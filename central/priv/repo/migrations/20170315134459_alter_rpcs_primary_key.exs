defmodule Central.Repo.Migrations.AlterRpcsPrimaryKey do
  use Ecto.Migration

  def up() do
    alter table(:air_rpcs) do
      modify(:id, :text)
    end
  end

  def down() do
    # We need to remove entries since some IDs might not be convertible anymore
    Ecto.Adapters.SQL.query!(Central.Repo, "truncate table air_rpcs", [])

    alter table(:air_rpcs) do
      modify(:id, :string)
    end
  end
end
