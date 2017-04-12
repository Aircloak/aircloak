defmodule IntegrationTest.TableauTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  setup context do
    {:ok, conn} = connect(context.user)
    {:ok, Map.put(context, :conn, conn)}
  end

  test "tableau table list query", context do
    query =
      'BEGIN;' ++
      'declare "SQL_CUR04645D10" cursor for ' ++
      'select relname, nspname, relkind from pg_catalog.pg_class c, pg_catalog.pg_namespace n ' ++
      'where relkind in (\'r\', \'v\') and ' ++
      'nspname not in (\'pg_catalog\', \'information_schema\', \'pg_toast\', \'pg_temp_1\') and ' ++
      'n.oid = relnamespace order by nspname, relname;' ++
      'fetch 2048 in "SQL_CUR04645D10";'

    assert :odbc.sql_query(context.conn, query) == [
      {:updated, 0},
      {:updated, 0},
      {:selected, ['relname', 'nspname', 'relkind'], [{'users', 'public', 'r'}]}
    ]
  end

  test "tableau table info query", context do
    query =
      'select n.nspname, c.relname, a.attname, a.atttypid, t.typname, a.attnum, a.attlen, a.atttypmod, ' ++
      'a.attnotnull, c.relhasrules, c.relkind, c.oid, pg_get_expr(d.adbin, d.adrelid), case t.typtype ' ++
      'when \'d\' then t.typbasetype else 0 end, t.typtypmod, c.relhasoids from (((pg_catalog.pg_class c ' ++
      'inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname like \'users\' ' ++
      'and n.nspname like \'public\') inner join pg_catalog.pg_attribute a on (not a.attisdropped) and ' ++
      'a.attnum > 0 and a.attrelid = c.oid) inner join pg_catalog.pg_type t on t.oid = a.atttypid) left ' ++
      'outer join pg_attrdef d on a.atthasdef and d.adrelid = a.attrelid and d.adnum = a.attnum order by ' ++
      'n.nspname, c.relname, attnum'

    assert {
      :selected,
      [
        'nspname', 'relname', 'attname', 'atttypid', 'typname', 'attnum', 'attlen', 'atttypmod', 'attnotnull',
        'relhasrules', 'relkind', 'oid', 'pg_get_expr', 'case', 'typtypmod', 'relhasoids'
      ],
      [
        {'public', 'users', 'user_id', '25', 'text', 1, -1, -1, false, false, 'r', oid, [], '0', -1, false},
        {'public', 'users', 'name', '25', 'text', 2, -1, -1, false, false, 'r', oid, [], '0', -1, false},
        {'public', 'users', 'height', '20', 'int8', 3, 8, -1, false, false, 'r', oid, [], '0', -1, false}
      ]
    } = :odbc.sql_query(context.conn, query)
  end

  test "tableau query for triggers", context do
    query =
      'BEGIN;declare "SQL_CUR04D36638" cursor for SELECT\tpt.tgargs, \t\tpt.tgnargs, \t\tpt.tgdeferrable, ' ++
      '\t\tpt.tginitdeferred, \t\tpp1.proname, \t\tpp2.proname, \t\tpc.oid, \t\tpc1.oid, \t\tpc1.relname, ' ++
      '\t\tpt.tgconstrname, pn.nspname FROM\tpg_catalog.pg_class pc, \t\tpg_catalog.pg_proc pp1, ' ++
      '\t\tpg_catalog.pg_proc pp2, \t\tpg_catalog.pg_trigger pt1, \t\tpg_catalog.pg_trigger pt2, ' ++
      '\t\tpg_catalog.pg_proc pp, \t\tpg_catalog.pg_trigger pt, \t\tpg_catalog.pg_class pc1, ' ++
      '\t\tpg_catalog.pg_namespace pn, \t\tpg_catalog.pg_namespace pn1 WHERE\tpt.tgrelid = pc.oid AND ' ++
      'pp.oid = pt.tgfoid AND pt1.tgconstrrelid = pc.oid AND pp1.oid = pt1.tgfoid AND pt2.tgfoid = pp2.oid ' ++
      'AND pt2.tgconstrrelid = pc.oid AND ((pc.relname = \'accounts\') AND (pn1.oid = pc.relnamespace) AND ' ++
      '(pn1.nspname = \'public\') AND (pp.proname LIKE \'%ins\') AND (pp1.proname LIKE \'%upd\') AND ' ++
      '(pp1.proname not LIKE \'%check%\') AND (pp2.proname LIKE \'%del\') AND (pt1.tgrelid=pt.tgconstrrelid) ' ++
      ' AND (pt1.tgconstrname=pt.tgconstrname) AND (pt2.tgrelid=pt.tgconstrrelid) AND ' ++
      '(pt2.tgconstrname=pt.tgconstrname) AND (pt.tgconstrrelid=pc1.oid) AND (pc1.relnamespace=pn.oid)) ' ++
      'order by pt.tgconstrname;fetch 2048 in "SQL_CUR04D36638"'

    assert :odbc.sql_query(context.conn, query) == [
      {:updated, 0},
      {:updated, 0},
      {:selected,
        ['tgargs', 'tgnargs', 'tgdeferrable', 'tginitdeferred', 'pp1.proname', 'pp2.proname', 'pc.oid',
          'pc1.oid', 'relname', 'tgconstrname', 'nspname'],
        []
      }
    ]
  end

  test "tableau query for related fields", context do
    query =
      'select n.nspname, c.relname, a.attname, a.atttypid, t.typname, a.attnum, a.attlen, a.atttypmod, ' ++
      'a.attnotnull, c.relhasrules, c.relkind, c.oid, pg_get_expr(d.adbin, d.adrelid), case t.typtype when ' ++
      '\'d\' then t.typbasetype else 0 end, t.typtypmod, c.relhasoids from (((pg_catalog.pg_class c inner ' ++
      'join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname = \'accounts\' and n.nspname = ' ++
      '\'public\') inner join pg_catalog.pg_attribute a on (not a.attisdropped) and a.attnum > 0 and ' ++
      'a.attrelid = c.oid) inner join pg_catalog.pg_type t on t.oid = a.atttypid) left outer join pg_attrdef ' ++
      'd on a.atthasdef and d.adrelid = a.attrelid and d.adnum = a.attnum order by n.nspname, c.relname, attnum'
    assert :odbc.sql_query(context.conn, query) == {
      :selected,
      ['nspname', 'relname', 'attname', 'atttypid', 'typname', 'attnum', 'attlen', 'atttypmod',
        'attnotnull', 'relhasrules', 'relkind', 'oid', 'pg_get_expr', 'case', 'typtypmod', 'relhasoids'],
      []
    }
  end

  test "tableau query for indexed columns", context do
    query =
      'BEGIN;declare "SQL_CUR04AD8270" cursor for select ta.attname, ia.attnum, ic.relname, n.nspname, ' ++
      'tc.relname from pg_catalog.pg_attribute ta, pg_catalog.pg_attribute ia, pg_catalog.pg_class tc, ' ++
      'pg_catalog.pg_index i, pg_catalog.pg_namespace n, pg_catalog.pg_class ic where tc.relname = ' ++
      '\'accounts\' AND n.nspname = \'public\' AND tc.oid = i.indrelid AND n.oid = tc.relnamespace AND ' ++
      'i.indisprimary = \'t\' AND ia.attrelid = i.indexrelid AND ta.attrelid = i.indrelid AND ta.attnum = ' ++
      'i.indkey[ia.attnum-1] AND (NOT ta.attisdropped) AND (NOT ia.attisdropped) AND ic.oid = i.indexrelid ' ++
      'order by ia.attnum;fetch 2048 in "SQL_CUR04AD8270"'
    assert :odbc.sql_query(context.conn, query) == [
      {:updated, 0}, {:updated, 0},
      {:selected, ['attname', 'attnum', 'relname', 'nspname', 'relname'], []}
    ]
  end

  defp connect(user, params \\ []) do
    params = Keyword.merge(
      [
        user: user.email,
        password: Manager.user_password(),
        database: Manager.data_source_global_id(),
        sslmode: "require"
      ],
      params
    )

    connection_string =
      %{
        "DSN" => "PostgreSQL",
        "Server" => "localhost",
        "Port" => Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
        "sslmode" => params[:sslmode],
        "Uid" => params[:user],
        "Pwd" => params[:password],
        "Database" => params[:database]
      }
      |> Enum.map(fn({name, value}) -> "#{name}=#{value};" end)
      |> Enum.join()
      |> to_charlist()

    :odbc.connect(connection_string, [])
  end
end
