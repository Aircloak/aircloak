defmodule IntegrationTest.ShadowDbTest do
  use ExUnit.Case, async: false

  alias IntegrationTest.Manager
  import Aircloak.AssertionHelper
  import IntegrationTest.Helpers

  # The following query is an intercepted \dt request.
  @dt_query """
    SELECT
      n.nspname AS "Schema",
      c.relname AS "Name",
      CASE c.relkind
        WHEN 'r' THEN 'table'
        WHEN 'v' THEN 'view'
        WHEN 'm' THEN 'materialized view'
        WHEN 'i' THEN 'index'
        WHEN 'S' THEN 'sequence'
        WHEN 's' THEN 'special'
        WHEN 'f' THEN 'foreign table'
        WHEN 'p' THEN 'partitioned table'
        WHEN 'I' THEN 'partitioned index'
      END AS "Type",
      pg_catalog.Pg_get_userbyid(c.relowner) AS "Owner"
    FROM
      pg_catalog.pg_class c
      LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
    WHERE
      c.relkind IN ('r', 'p', '')
      AND n.nspname <> 'pg_catalog'
      AND n.nspname <> 'information_schema'
      AND n.nspname !~ '^pg_toast'
      AND pg_catalog.Pg_table_is_visible(c.oid)
    ORDER BY 1, 2;
  """

  setup do
    user = Manager.create_admin_user()
    {:ok, conn} = connect(user)
    [user: user, conn: conn]
  end

  describe "psql interface's shadow db" do
    test "Analyst tables should be listed amongst tables", context do
      name = unique_name(:table)

      refute table_exists?(context.conn, name)

      assert {:ok, _table} = create_analyst_table(context.user, name, "select user_id, name from users")

      assert table_exists?(context.conn, name)

      assert [
               ["user_id", "text"],
               ["name", "text"]
             ] = table_columns(context.conn, name)
    end

    test "Analyst table updates should reflect in the shadow db", context do
      name = unique_name(:table)
      new_name = unique_name(:table)

      assert {:ok, table} = create_analyst_table(context.user, name, "select user_id, name from users")

      assert {:ok, _} =
               Air.Service.AnalystTable.update(table.id, context.user, new_name, "select user_id from users", "comment")

      assert not table_exists?(context.conn, name)
      assert table_exists?(context.conn, new_name)
      assert [["user_id", "text"]] = table_columns(context.conn, new_name)
    end

    test "Views should be listed amongst tables", context do
      name = unique_name(:view)

      assert not table_exists?(context.conn, name)

      assert {:ok, _view} = create_view(context.user, name, "select user_id, name from users")

      assert table_exists?(context.conn, name)

      assert [
               ["user_id", "text"],
               ["name", "text"]
             ] = table_columns(context.conn, name)
    end

    test "View updates should reflect in the shadow db", context do
      name = unique_name(:view)
      new_name = unique_name(:view)

      assert {:ok, view} = create_view(context.user, name, "select user_id, name from users")
      assert {:ok, _} = Air.Service.View.update(view.id, context.user, new_name, "select user_id from users", "comment")

      assert not table_exists?(context.conn, name)
      assert table_exists?(context.conn, new_name)
      assert [["user_id", "text"]] = table_columns(context.conn, new_name)
    end

    test "Recreating a shadow db based on schema changes from cloak should also include selectables", context do
      table_name = unique_name(:table)
      view_name = unique_name(:view)

      assert {:ok, _table} = create_analyst_table(context.user, table_name, "select user_id, name from users")
      assert {:ok, _view} = create_view(context.user, view_name, "select user_id, name from users")

      assert table_exists?(context.conn, table_name)
      assert table_exists?(context.conn, view_name)

      Cloak.DataSource.reinitialize_all_data_sources()
      Manager.restart_cloak()

      # Wait for updates to settle.

      assert_soon table_exists?(context.conn, table_name)
      assert_soon table_exists?(context.conn, view_name)
    end
  end

  defp create_analyst_table(user, name, sql) do
    Air.Service.AnalystTable.create(user, Manager.data_source(), name, sql, "analyst table comment")
  end

  defp create_view(user, name, sql) do
    Air.Service.View.create(user, Manager.data_source(), name, sql, "view comment")
  end

  defp table_columns(conn, table_name) do
    result =
      Postgrex.query!(
        conn,
        """
        SELECT
          a.attname as "Column",
          pg_catalog.format_type(a.atttypid, a.atttypmod) as "Datatype"
        FROM
          pg_catalog.pg_attribute a
        WHERE
          a.attnum > 0
          AND NOT a.attisdropped
          AND a.attrelid = (
              SELECT c.oid
              FROM pg_catalog.pg_class c
                  LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
              WHERE c.relname = $1
                  AND pg_catalog.pg_table_is_visible(c.oid)
          );
        """,
        [table_name]
      )

    result.rows
  end

  defp table_exists?(conn, table_name) do
    # We simulate a timeout to get the shadow db to clear its cache.
    send(Air.PsqlServer.ShadowDb, :timeout)

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
