-module(db_test_helpers).

-compile(export_all).

simple_query(Query) ->
  air_db:call(fun(Connection) -> pgsql_connection:simple_query(Query, Connection) end).

batch_query(Query, Params) ->
  air_db:call(fun(Connection) -> pgsql_connection:batch_query(Query, Params, Connection) end).

insert_rows(Table, Fields, Rows) ->
  {Params, _} = lists:foldl(
        fun(_, {Acc, Index}) -> {[[$$, Index + $0] | Acc], Index + 1} end,
        {[], 1},
        Fields
      ),
  Query = io_lib:format("INSERT INTO ~s (~s) VALUES(~s)",
      [Table, string:join(Fields, ","), string:join(lists:reverse(Params), ",")]),
  [{{insert, 0, 1}, _} = Result || Result <- batch_query(Query, Rows)],
  ok.

last_id(Table) ->
  {{select, 1}, [{Id}]} = db_test_helpers:simple_query(
      ["SELECT currval(pg_get_serial_sequence('", Table, "','id'));"]),
  Id.