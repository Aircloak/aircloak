-module(db_test_helpers).

-compile(export_all).

simple_query(Query) ->
  air_db:call(fun(Connection) -> sql_conn:simple_query(Query, Connection) end).

extended_query(Query, Params) ->
  air_db:call(fun(Connection) -> sql_conn:extended_query(Query, Params, Connection) end).

batch_query(Query, Params) ->
  air_db:call(fun(Connection) -> sql_conn:batch_query(Query, Params, Connection) end).

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

http_url(Url) ->
  lists:flatten(io_lib:format("http://127.0.0.1:~s~s", [
      air_etcd:get("/tcp_ports/air_backend/http"), Url])).