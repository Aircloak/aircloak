defmodule Air.Repo.Migrations.ClearAnalystTables do
  use Ecto.Migration

  # The registration data format has been changed, so we need to drop previously stored tables.
  def up, do: execute("truncate table analyst_tables")
  def down, do: execute("truncate table analyst_tables")
end
