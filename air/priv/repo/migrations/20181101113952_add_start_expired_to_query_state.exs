defmodule Air.Repo.Migrations.AddStartExpiredToQueryState do
  use Ecto.Migration
  @disable_ddl_transaction true

  def up do
    execute("ALTER TYPE query_state ADD VALUE IF NOT EXISTS 'start_expired'")
  end

  def down do
    # a down migration won't remove the enum value, since PostgreSQL doesn't support it
    :ok
  end
end
