%% @doc This is a test module to test the db operations (creating a table, removing a table, adding data).
%%      We check correctness by manually inspecting all the tables in the database.
%% @end
-module(full_db_test).

-include("deps/proper/include/proper.hrl").
-include("test/full_test.hrl").
-include("test/prop_tracer.hrl").

%% API
-export([
  prop_db/5
]).

%% Property state machine callbacks
-export([
  command/1,
  initial_state/0,
  next_state/3,
  precondition/2,
  postcondition/3
]).


%% -------------------------------------------------------------------
%% Internal state representation and initial state
%% -------------------------------------------------------------------

-record(state, {
  db_state :: db_model:db_state(),
  cloak_node :: node()
}).

initial_state() ->
  #state{
    db_state = db_model:create(?USERS, ?VALUES, ?MAX_TABLES, ?MAX_INITIAL_COLUMNS),
    cloak_node = proper_cloak_instance:default_cloak_node()
  }.


%% -------------------------------------------------------------------
%% Starting PropEr
%% -------------------------------------------------------------------

prop_db(Users, Values, MaxTables, MaxInitialColumns, CommandsFactor) ->
  CloakNode = proper_cloak_instance:default_cloak_node(),
  InitialState = #state{
    db_state = db_model:create(Users, Values, MaxTables, MaxInitialColumns),
    cloak_node = CloakNode
  },
  ?FORALL(Commands, more_commands(CommandsFactor, proper_statem:commands(?MODULE, InitialState)),
      ?IMPLIES(db_model:command_list_has_at_least_one_add_data(Commands),
          ?TRACEFAIL(Commands,
                begin
                  proper_cloak_instance:stop_instance(),
                  proper_cloak_instance:start_instance(),
                  proper_statem:run_commands(?MODULE, Commands)
                end,
                measure("Command sequence length", length(Commands),
                    aggregate(my_command_names(Commands), Result =:= ok))

              ))).

my_command_names(Commands) ->
  [my_command_name(Command) || {set, {var, _}, Command} <- Commands].

my_command_name({call, rpc, call, [_Node, Module, Function, Parameters]}) ->
  {Module, Function, length(Parameters)};
my_command_name(_) ->
  unknown.


%% -------------------------------------------------------------------
%% PropEr state machine callbacks
%% -------------------------------------------------------------------

command(#state{db_state=DbState, cloak_node=CloakNode}) ->
  frequency(db_model:command_list(1, CloakNode, DbState)).

next_state(#state{db_state=DbState}=State, Result, Call) ->
  State#state{db_state=db_model:next_state(DbState, Result, Call)}.

precondition(#state{db_state=DbState}, Call) ->
  true =:= db_model:precondition(DbState, Call).

postcondition(#state{db_state=DbState}, Result, Call) ->
  true =:= db_model:postcondition(DbState, Result, Call).
