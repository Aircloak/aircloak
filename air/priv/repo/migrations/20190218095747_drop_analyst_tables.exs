defmodule Air.Repo.Migrations.DropAnalystTables do
  use Ecto.Migration

  def up, do: execute("delete from user_selectables where type='analyst_table'")

  def down, do: :ok
end
