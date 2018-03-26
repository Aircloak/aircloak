defmodule Air.Repo.Migrations.AddNewContextEnumTypeToQueries do
  use Ecto.Migration
  # This approach to extending the Postgres enum is taken from the ecto_enum
  # docs [1]. The problem being that extending the enum cannot be done within
  # a transaction (hence the disabling). Likewise the dropping of the enum
  # value is not natively possible in Postgres [2]. Therefore we ignore the
  # down migration altogether.
  #
  # 1: https://github.com/gjaldon/ecto_enum#postgres
  # 2: https://stackoverflow.com/questions/25811017/how-to-delete-an-enum-type-in-postgres
  @disable_ddl_transaction true

  def up do
    Ecto.Migration.execute("ALTER TYPE query_context ADD VALUE 'api'")
  end

  def down do
  end
end
