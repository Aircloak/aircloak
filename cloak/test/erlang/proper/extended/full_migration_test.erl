%% @doc This is a test module to test the migration operations (joining a node and removing a node).
%%      We check correctness by checking the ring on all active nodes.
%% @end
-module(full_migration_test).

-include("deps/proper/include/proper.hrl").
-include("full_test.hrl").
-include("debug_helpers.hrl").

%% API
-export([
  prop_migration/5
]).

%% Property state machine callbacks
-export([
  command/1,
  initial_state/0,
  next_state/3,
  precondition/2,
  postcondition/3
]).

%% State machine callbacks
-export([
  leave_node/2,
  join_node/2
]).


%% -------------------------------------------------------------------
%% Internal state representation and initial state
%% -------------------------------------------------------------------

-record(state, {
  db_state :: db_model:db_state(),
  cloak_node :: node(),
  other_cloak_node :: node(),
  joined = false :: boolean()
}).

initial_state() ->
  #state{
    db_state = db_model:create(?USERS, ?VALUES, ?MAX_TABLES, ?MAX_INITIAL_COLUMNS),
    cloak_node = proper_cloak_instance:cloak_node(1),
    other_cloak_node = proper_cloak_instance:cloak_node(2)
  }.


%% -------------------------------------------------------------------
%% Starting PropEr
%% -------------------------------------------------------------------

prop_migration(Users, Values, MaxTables, MaxInitialColumns, CommandsFactor) ->
  CloakNode = proper_cloak_instance:cloak_node(1),
  OtherCloakNode = proper_cloak_instance:cloak_node(2),
  InitialState = #state{
    db_state = db_model:create(Users, Values, MaxTables, MaxInitialColumns),
    cloak_node = CloakNode,
    other_cloak_node = OtherCloakNode
  },
  ?FORALL(Commands, more_commands(CommandsFactor, proper_statem:commands(?MODULE, InitialState)),
      ?IMPLIES(
          db_model:command_list_has_at_least_one_add_data(Commands) andalso
              command_list_has_at_least_two_joins(Commands),
          ?TRAPEXIT(
              begin
                proper_cloak_instance:stop_instance(2),
                proper_cloak_instance:stop_instance(1),
                {ok, CloakNode} = proper_cloak_instance:start_instance(1),
                {History, State, Result} = proper_statem:run_commands(?MODULE, Commands),
                ?WHENFAIL(
                      io:format("History: ~p\nState: ~p\nCommands: ~p\nResult: ~p\nLast command: ~p\n",
                          [History, State, Commands, Result, lists:nth(length(History), Commands)]),
                      measure("Command sequence length", length(Commands),
                          aggregate(my_command_names(Commands), Result =:= ok))
                    )
              end))).

my_command_names(Commands) ->
  [my_command_name(Command) || {set, {var, _}, Command} <- Commands].

my_command_name({call, rpc, call, [_Node, Module, Function, Parameters]}) ->
  {Module, Function, length(Parameters)};
my_command_name({call, ?MODULE, Function, Parameters}) ->
  {?MODULE, Function, length(Parameters)};
my_command_name(_) ->
  unknown.


%% -------------------------------------------------------------------
%% PropEr state machine callbacks
%% -------------------------------------------------------------------

command(#state{db_state=DbState, cloak_node=CloakNode, other_cloak_node=OtherNode, joined=Joined}) ->
  frequency(
    db_model:command_list(1, CloakNode, DbState) ++
    [
      {1, {call, ?MODULE, leave_node, [CloakNode, OtherNode]}}
    ||
      IsJoined <- [Joined],
      IsJoined
    ] ++
    [
      {1, {call, ?MODULE, join_node, [CloakNode, OtherNode]}}
    ||
      IsJoined <- [Joined],
      not IsJoined
    ]
  ).

next_state(State, _Result, {call, ?MODULE, leave_node, [_, _]}) ->
  State#state{joined=false};
next_state(State, _Result, {call, ?MODULE, join_node, [_, _]}) ->
  State#state{joined=true};
next_state(#state{db_state=DbState}=State, Result, Call) ->
  State#state{db_state=db_model:next_state(DbState, Result, Call)}.

precondition(#state{joined=Joined}, {call, ?MODULE, leave_node, [_, _]}) ->
  Joined;
precondition(#state{joined=Joined}, {call, ?MODULE, join_node, [_, _]}) ->
  not Joined;
precondition(#state{db_state=DbState}, Call) ->
  true =:= db_model:precondition(DbState, Call).

postcondition(#state{cloak_node=Node}, {call, ?MODULE, leave_node, [_, _]}, Result) ->
  Result =:= ok andalso
      ring:nodes(rpc:call(Node, ring_state, active_ring, [])) == [Node];
postcondition(#state{cloak_node=Node, other_cloak_node=Other},
    {call, ?MODULE, join_node, [_, _]}, Result) ->
  Result =:= ok andalso
      lists:sort(ring:nodes(rpc:call(Node, ring_state, active_ring, []))) == lists:sort([Node, Other]) andalso
      lists:sort(ring:nodes(rpc:call(Other, ring_state, active_ring, []))) == lists:sort([Node, Other]);
postcondition(#state{db_state=DbState}, Call, Result) ->
  true =:= db_model:postcondition(DbState, Call, Result).


%% -------------------------------------------------------------------
%% State machine callbacks
%% -------------------------------------------------------------------

leave_node(Node, OtherNode) ->
  ok = rpc:call(Node, joinleave_manager, leave, [OtherNode]),
  wait_for_joinleave_manager(Node),
  rpc:call(Node, migration_manager, migrate, []),
  wait_for_migration_manager(Node),
  proper_cloak_instance:stop_instance(2),
  proper_cloak_instance:stop_instance(1),
  proper_cloak_instance:start_instance(1, false),
  ok.

join_node(Node, OtherNode) ->
  proper_cloak_instance:start_instance(2),
  request_join_node(Node, OtherNode, 100),
  wait_for_joinleave_manager(Node),
  rpc:call(Node, migration_manager, migrate, []),
  wait_for_migration_manager(Node),
  wait_for_migration_manager(OtherNode),
  ok.

request_join_node(_Node, OtherNode, 0) ->
  throw({join_failed, OtherNode});
request_join_node(Node, OtherNode, N) ->
  case rpc:call(OtherNode, joinleave_manager, join, [Node]) of
    ok ->
      ok;
    _ ->
      timer:sleep(250),
      request_join_node(Node, OtherNode, N-1)
  end.

wait_for_migration_manager(Node) ->
  wait_for_migration_manager(Node, 2500).

wait_for_migration_manager(Node, 0) ->
  throw({migration_failed, Node, rpc:call(Node, migration_manager, changes_after_migration, [])});
wait_for_migration_manager(Node, N) ->
  case rpc:call(Node, migration_manager, changes_after_migration, []) of
    {[], []} ->
      %% No further nodes should join or leave...  So we have finished the migration.
      ok;
    {_, _} ->
      %% Migration was canceled for some reason.  So we hit one of the race conditions.
      %% We can live with that but need to restart migration here otherwise we have to wait some minutes
      %% for migration_daemon to trigger the next migration...
      rpc:call(Node, migration_manager, migrate, []),
      wait_for_migration_manager(Node);
    _Otherwise ->
      %% Output something to make Travis see progress...
      io:format(standard_error, ",", []),
      timer:sleep(1000),
      wait_for_migration_manager(Node, N-1)
  end.

wait_for_joinleave_manager(Node) ->
  wait_for_joinleave_manager(Node, 1000).

wait_for_joinleave_manager(Node, 0) ->
  throw({migration_failed, Node});
wait_for_joinleave_manager(Node, N) ->
  case rpc:call(Node, joinleave_manager, get_joins_and_leaves, []) of
    {JoinSet, LeaveSet} ->
      case sets:size(JoinSet) + sets:size(LeaveSet) > 0 of
        true ->
          ok;
        false ->
          timer:sleep(250),
          wait_for_joinleave_manager(Node, N-1)
      end;
    _Otherwise ->
      timer:sleep(250),
      wait_for_joinleave_manager(Node, N-1)
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

command_list_has_at_least_two_joins(Commands) ->
  length(lists:filter(
        fun
          ({set, {var, _}, {call, ?MODULE, join_node, _}}) -> true;
          (_) -> false
        end,
        Commands
      )) > 0.
