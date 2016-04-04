%% -------------------------------------------------------------------
%% Datatypes for data about users
%% -------------------------------------------------------------------

%% The following datatypes describe how we internally encode
%% data to be inserted into, or read from, Postgres.
%% As a common and standardized format it can be used
%% throughout different components, including:
%%
%% - the data validation tool which verifies that
%%   the data follows the format of the tables in the
%%   database
%% - the data insertion tool, which has to create
%%   valid SQL statements to be executed in the database
%% - the sandbox managers that need to communicate
%%   data between the sandboxed tasks and the
%%   insert tool, and from the database back to
%%   the sandboxed task.
%%

-type data_value() :: binary() | number() | boolean() | nil.

-type schema_name() :: binary().
-type table_name() :: binary().

-type column_name() :: binary().
-type column_spec() :: {columns, [column_name()]}.

-type row_data() :: [data_value()].
-type column_data() :: {data, [row_data()]}.

-type table_data() :: {table_name(), [column_spec() | column_data()]}.

-type user_data() :: [table_data()].

%% type for the timestamp field
-type datetime() :: {calendar:date(), {0..23, 0..59, 0..59 | float()}}.
