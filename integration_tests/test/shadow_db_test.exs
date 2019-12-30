defmodule IntegrationTest.ShadowDbTest do
  use ExUnit.Case, async: false

  alias IntegrationTest.Manager
  import Aircloak.AssertionHelper
  import IntegrationTest.Helpers

  # The following query is an intercepted \dt request.
  @dt_query "SELECT n.nspname as \"Schema\", c.relname as \"Name\", CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'm' THEN 'materialized view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' WHEN 'f' THEN 'foreign table' WHEN 'p' THEN 'partitioned table' WHEN 'I' THEN 'partitioned index' END as \"Type\", pg_catalog.pg_get_userbyid(c.relowner) as \"Owner\" FROM pg_catalog.pg_class c LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace WHERE c.relkind IN ('r','p','') AND n.nspname <> 'pg_catalog' AND n.nspname <> 'information_schema' AND n.nspname !~ '^pg_toast' AND pg_catalog.pg_table_is_visible(c.oid) ORDER BY 1,2;"

  setup do
    Air.PsqlServer.ShadowDb.SchemaSynchronizer.enable_events()

    on_exit(fn ->
      Air.PsqlServer.ShadowDb.SchemaSynchronizer.disable_events()
    end)

    user = Manager.create_admin_user()
    {:ok, conn} = connect(user)
    [user: user, conn: conn]
  end

  describe "psql interface's shadow db" do
    test "Analyst tables should be listed amongst tables", context do
      name = unique_name(:table)

      assert not table_exists?(context.conn, name)

      assert {:ok, _table} = create_analyst_table(context.user, name, "select user_id, name from users")

      assert soon(table_exists?(context.conn, name))
    end

    test "Views should be listed amongst tables", context do
      name = unique_name(:view)

      assert not table_exists?(context.conn, name)

      assert {:ok, _view} = create_view(context.user, name, "select user_id, name from users")

      assert soon(table_exists?(context.conn, name))
    end
  end

  defp create_analyst_table(user, name, sql) do
    Air.Service.AnalystTable.create(user, Manager.data_source(), name, sql)
  end

  defp create_view(user, name, sql) do
    Air.Service.View.create(user, Manager.data_source(), name, sql)
  end

  defp table_exists?(conn, table_name) do
    result = Postgrex.query!(conn, @dt_query, [])

    result.rows
    |> Enum.any?(fn [_schema, name, _type, _owner] -> name == table_name end)
  end

  defp connect(user) do
    Postgrex.start_link(
      hostname: "localhost",
      port: Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
      username: Manager.login(user),
      password: Manager.user_password(),
      database: Manager.data_source_name(),
      ssl: true
    )
  end
end
