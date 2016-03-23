%% @doc HTTP API endpoint for inserting data.
%%
%%      Example asynchronous and synchronous requests:
%%
%%        - synchronous request (the default value of async_query is false, so we can omit the header when
%%          running a synchronous request):
%%
%%        ```
%%          cat << EOF | \
%%            curl http://localhost:8098/task/run -v \
%%                 --header "async_query:false" \
%%                 --data @-
%%            {
%%              "prefetch": [
%%                {
%%                  "table": "test1",
%%                  "where": {"\$\$priority": {"$lt": 3}}
%%                }
%%              ],
%%              "post_processing": {
%%                "code":"report_property(tables.test1[0].name, tables.test1[0].priority)",
%%
%%                // libraries are optional
%%                "libraries":[
%%                  {"name": "lib1", "code": "function foo() ... end"},
%%                  {"name": "lib2", "code": "function bar() ... end"}
%%                ]
%%              }
%%            }
%%          EOF
%%        '''
%%
%%        - asynchronous request (requires additional headers task_id and auth_token, can have optional
%%          header return_url - in base64)
%%          On successful task initialization it will return a handle to be used to query the progress of the
%%          task.
%%
%%        ```
%%          cat << EOF | \
%%            curl http://localhost:8098/task/run -v \
%%                 --header "task_id:QWlyY2xvYWsgUm9ja3M=" \
%%                 --header "async_query:true" \
%%                 --header "auth_token:vKVEcuMs3pVd2bFZgT0VZA" \
%%                 --header "return_url:aHR0cHM6Ly9oZWxsby5haXJjbG9hay5jb20vcmVzdWx0cw==" \
%%                 --data @-
%%            {
%%              "prefetch": [
%%                {
%%                  "table": "test1",
%%                  "where": {"\$\$priority": {"$lt": 3}}
%%                }
%%              ],
%%              "post_processing": {
%%                "code":"report_property(tables.test1[0].name, tables.test1[0].priority)"
%%              }
%%            }
%%          EOF
%%        '''
%%
%%        - getting progress of an asynchronous batch query (GET request)
%%
%%        ```
%%          curl http://localhost:8098/task/<progress-handle> -v
%%        '''
%%
%%        It should return a JSON object with the element "progress" indicating the progress in percent (as
%%        an anonymized integer value).
-module(task_resource).

%% webmachine callbacks
-export([
  init/1,
  allowed_methods/2,
  content_types_provided/2,
  is_authorized/2,
  post_is_create/2,
  process_post/2,
  process_get/2
]).

-include("cloak.hrl").
-include_lib("webmachine/include/webmachine.hrl").


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) -> {ok, nostate}.

%% @hidden
allowed_methods(Req, State) -> {['POST', 'GET'], Req, State}.

content_types_provided(ReqData, State) ->
  {[{"application/json", process_get}], ReqData, State}.

%% @hidden
is_authorized(Req, State) ->
  {true, Req, State}.

%% @hidden
post_is_create(Req, State) -> {false, Req, State}.

%% @hidden
process_post(Req, _State) ->
  handle_action(post, wrq:path_info(action, Req), Req).

%% @hidden
process_get(Req, _State) ->
  handle_action(get, wrq:path_info(action, Req), Req).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

handle_action(post, "run", Req) ->
  resource_common:stop_on_error(Req, undefined, [
    % first check if we have a asynchronous query (default: false)
    fun([]) ->
      case  wrq:get_req_header("async_query", Req) of
        undefined -> {ok, sync_query};
        "true" -> {ok, async_query};
        "false" -> {ok, sync_query};
        _ -> {error, "unsupported async_query header value"}
      end
    end,

    % get the auth_token for asynchronous queries
    % first get the required header task_id
    fun
      ([async_query]) -> resource_common:get_required_headers(["task_id", "auth_token"], Req);
      (_) -> {ok, {"task", undefined}}
    end,

    % generate the correct reply token for asynchronous and synchronous queries
    fun
      ([{_TaskId, AuthToken}, async_query]) ->
        {ok, {json, {url, AuthToken, return_url(Req)}}};
      ([_, sync_query]) ->
        {ok, {json, {process, self()}}}
    end,

    % Decode the task.
    fun([ReturnToken, {TaskId, _}, _]) ->
      {ok, decode_task(TaskId, ReturnToken, Req)}
    end,

    % Add a progress report handler if we have an asynchronous query.
    fun
      ([Task, _, _, async_query]) ->
        {ok, progress_handler:register_task(Task, progress_url(Req))};
      ([Task, _, _, _]) ->
        {ok, Task}
    end,

    % just run the task (this always succeeds)
    fun([Task, _, _, _, _]) ->
      task_coordinator:run_task(Task)
    end,

    % reply via webmachine (wait for result if it is a synchronous query)
    fun
      ([Task, _, _, _, async_query]) ->
        % Return the progress report handle such that we can query the progress later.
        JSONResponse = case Task#task.progress_handle of
          undefined -> [{success, true}];
          Handle -> [{success, true}, {progress_handle, Handle}]
        end,
        {ok, {200, resource_common:respond_json(JSONResponse, Req)}};
      ([_, _, _, _, sync_query]) ->
        receive
          {reply, Result} ->
            {ok, {200, wrq:set_resp_body(Result, Req)}}
        after cloak_conf:get_val(queries, sync_query_timeout) ->
          ?WARNING("Synchronous query timed out"),
          {error, "The query timed out. Consider running it asynchronously if it is a slow task."}
        end
    end
  ]);
handle_action(get, EncodedHandle, Req) ->
  Handle = http_uri:decode(EncodedHandle),
  resource_common:stop_on_error(Req, undefined, [
    % Retrieve progress
    fun([]) ->
      progress_handler:get_progress(list_to_binary(Handle))
    end,

    % Generate JSON Reply
    fun([Progress]) ->
      {ok, {200, resource_common:respond_json([{success, true}, {progress, Progress}], Req)}}
    end
  ]);
handle_action(_, _, Req) ->
  {{halt, 404}, resource_common:path_error(Req), undefined}.

return_url(Req) ->
  OverriderUrl = decode_base64_header(wrq:get_req_header("return_url", Req)),
  cloak_conf:get_val(air, return_url, OverriderUrl).

progress_url(Req) ->
  decode_base64_header(wrq:get_req_header("progress_url", Req)).

decode_base64_header(undefined) ->
  undefined;
decode_base64_header(Value) when is_list(Value) ->
  base64:decode_to_string(Value).

decode_task(TaskId, ReturnToken, Req) ->
  SourceIP = case {wrq:get_req_header("X-Real-IP", Req), wrq:get_req_header("X-Forwarded-For", Req)} of
    {undefined, undefined} ->
      ?WARNING("neither X-Real-IP nor X-Forwarded-For headers set;  using peer IP"),
      wrq:peer(Req);
    {undefined, ForwardedForIPs} ->
      ?WARNING("X-Real-IP header is missing"),
      ForwardedForIPs;
    {RealIP, undefined} ->
      ?WARNING("X-Forwarded-For header is missing"),
      RealIP;
    {RealIP, ForwardedForIPs} ->
      RealIP ++ "; " ++ ForwardedForIPs
  end,
  Task = ?MEASURE(
        "task.parse_json",
        task_json_parser:parse(list_to_binary(TaskId), SourceIP, wrq:req_body(Req))
      ),
  Task#task{return_token=ReturnToken}.
