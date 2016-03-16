/**
  % Lua sandbox
  % Aircloak
  % May 2014
*/

#define _POSIX_C_SOURCE   200809L

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

#include "lua_includes.h"

#include "job_communicator.h"
#include "sandbox_error.h"
#include "lua_datapb.h"

static int external_call(lua_State *L);
static int get_next_batch(lua_State *L);
static int get_batch_row(lua_State *L);
static int free_batch(lua_State *L);
static int to_date(lua_State *L);

/**
  # Auxiliary function to get a copy of a Lua string
*/

char *
get_lua_string(int stack_pos)
{
  char  *luastr, *str;

  if ((luastr = (char *)lua_tolstring(ls, stack_pos, NULL)) == NULL)
    throw("value is not a string", false);
  if ((str = strdup(luastr)) == NULL)
    throw("out of memory when duplicating string", false);

  return str;
}


/**
  # Reading the auxiliary code from disk
*/

static const char  *priv_lua_lib_files[] = {
  "priv/helpers.lua",
  "priv/serialization.lua",
  NULL
};

struct PrivLuaLib {
  size_t size;
  char *code;
  const char *name;
};

static struct PrivLuaLib **priv_lua_libs = NULL;

static
void
read_helpers_lua(void)
{
  struct stat  st;
  int          fd;
  size_t       pos = 0;
  ssize_t      res;
  int num_libs = 0;
  const char **plib_name = NULL;

  for (plib_name = priv_lua_lib_files; *plib_name != NULL; plib_name++)
    num_libs++;

  /* Allocate one more element to hold the NULL */
  if ((priv_lua_libs = malloc(sizeof(struct PrivLuaLib *) * (num_libs + 1))) == NULL)
    abort();
  priv_lua_libs[num_libs] = NULL;

  for (int n_lib = 0; n_lib < num_libs; n_lib++) {
    /* Allocate memory for this file */
    if ((priv_lua_libs[n_lib] = malloc(sizeof(struct PrivLuaLib))) == NULL)
      abort();

    priv_lua_libs[n_lib]->name = priv_lua_lib_files[n_lib];

    /* get size of file */
    if (stat(priv_lua_lib_files[n_lib], &st))
      abort();
    if (st.st_size > SSIZE_MAX)  /* XXX: what's off_t???  so this is a stupid hack;  for our purposes files
                                    larger than SSIZE_MAX on normal platforms should be avoided anyway */
      abort();  /* file is too large for memory */
    priv_lua_libs[n_lib]->size = (size_t)st.st_size;

    /* allocate memory to store the whole file */
    if ((priv_lua_libs[n_lib]->code = malloc(priv_lua_libs[n_lib]->size)) == NULL)
      abort();

    /* open the file */
    if ((fd = open(priv_lua_lib_files[n_lib], O_RDONLY)) == -1)
      abort();

    /* read the whole file contents */
    pos = 0;
    do {
      res = read(fd, priv_lua_libs[n_lib]->code + pos, priv_lua_libs[n_lib]->size - pos);
      if (res > 0)
        pos += (size_t)res;
    } while ((res > 0 && (priv_lua_libs[n_lib]->size - pos) > 0) || (res < 0 && (errno == EINTR || errno == EAGAIN)));

    if (res <= 0)  /* we should never see the EOF, so encountering it is a bug! */
      abort();  /* cannot read helpers, so break */

    if (close(fd))
      abort();  /* cannot close file */
  }
}


/**
  # Auxiliary function for evaluating the top of stack closure
*/

static
void
do_lua_pcall(int nargs, int nresults) {
  switch (lua_pcall(ls, nargs, nresults, 0)) {
    case LUA_ERRRUN:
      {
        const char* error = get_lua_string(-1); /* duplicate the error message returned by Lua */
        lua_pop(ls, 1);  /* pop the error message from the stack */
        throw(error, true); /* return the error message */
      }
      break;

    case LUA_ERRMEM:
      throw("out of memory when running code", false);
      break;

    case LUA_ERRERR:
      throw("error while running the error handler", false);
      break;
  }
}

/**
  # Loads custom library sources into the context
*/

static
void
load_libraries() {
  for (size_t i = 0; i < job_create_message->job_request->n_libraries; i++) {
    Aircloak__LibraryDataPB *library = job_create_message->job_request->libraries[i];
    if (luaL_loadbuffer(ls, (char *) library->code.data, library->code.len, library->name) != 0) {
      int size = snprintf(NULL, 0, "cannot load library %s", library->name);
      dynamic_error_message = (char *) malloc((size + 1) * sizeof(char));
      snprintf(dynamic_error_message, size + 1, "cannot load library %s", library->name);
      throw(dynamic_error_message, false);
    }

    /* setup reduced environment */
    lua_getglobal(ls, "job_environment");
    if (!lua_setfenv(ls, -2))
      throw("cannot set environment", false);

    do_lua_pcall(0, LUA_MULTRET);
  }
}


/**
  # Load and execute in reduced state
*/

static const char  *objects_to_copy[] = {
  "assert",
  "collectgarbage",
  "error",
  "ipairs",
  "loadstring",
  "next",
  "pairs",
  "pcall",
  "rawequal",
  "rawget",
  "rawset",
  "select",
  "tonumber",
  "tostring",
  "type",
  "unpack",
  "xpcall",
  "math",
  "string",
  "table",
  "report_property",
  "user_table",
  "load_user_table",
  "get_user_tables",
  "ac_external_call",
  "ac_get_next_batch",
  "to_date",
  "accumulator",
  "task_time",
  NULL
};

static
void
run_with_reduced_state(char *code, size_t code_length)
{
  const char  **cur = objects_to_copy;

  /* create new environment table which is used by all user-provided code */
  lua_newtable(ls);
  while (*cur != NULL) {
    lua_getglobal(ls, *cur);
    lua_setfield(ls, -2, *cur);
    cur++;
  }
  lua_setglobal(ls, "job_environment");

  /* load libraries */
  load_libraries();

  /* setup code */
  switch(luaL_loadbuffer(ls, code, code_length, "task_code")) {
    case LUA_ERRSYNTAX:
      {
        const char* error = get_lua_string(-1); /* duplicate the error message returned by Lua */
        lua_pop(ls, 1);  /* pop the error message from the stack */
        throw(error, true); /* return the error message */
      }
      break;

    case LUA_ERRMEM:
      throw("out of memory when loading code", false);
      break;
  }

  /* setup reduced environment */
  lua_getglobal(ls, "job_environment");
  if (!lua_setfenv(ls, -2))
    throw("cannot set environment", false);

  /* run the code */
  do_lua_pcall(0, 1);
  lua_settop(ls, 0);
}


/**
  # Creating the initial state

  When running a lua task we need to create the initial state containing the code and having the function for
  reading from the user's data file registered.
*/

static
void
create_lua_state(void)
{
  if ((ls = lua_open()) == NULL)
    throw("out of memory when creating state", false);
  luaopen_base(ls);
  luaopen_math(ls);
  luaopen_string(ls);
  luaopen_table(ls);
  lua_register(ls, "ac_external_call", external_call);
  lua_register(ls, "ac_get_next_batch", get_next_batch);
  lua_register(ls, "to_date", to_date);

  // create batch objects metatable
  luaL_newmetatable(ls, "batch_mt");
  // set batch objects finalizer
  lua_pushstring(ls, "__gc");
  lua_pushcfunction(ls, free_batch);
  lua_settable(ls, -3);
  // set batch objects indexer
  lua_pushstring(ls, "__index");
  lua_pushcfunction(ls, get_batch_row);
  lua_settable(ls, -3);
  // pop batch objects metatable
  lua_pop(ls, 1);

  for (struct PrivLuaLib **priv_lib = priv_lua_libs; *priv_lib != NULL; priv_lib++) {
    if (luaL_loadbuffer(ls, (*priv_lib)->code, (*priv_lib)->size, (*priv_lib)->name) != 0)
      abort();  /* critical error */
    do_lua_pcall(0, LUA_MULTRET);
    lua_settop(ls, 0);
  }
}


/**
  # Adding input to the global state
*/

#if INT_MAX < SIZE_MAX
static
void
aclua_createtable(size_t narr, size_t nrec)
{
  if ((size_t)INT_MAX < narr || (size_t)INT_MAX < nrec)
    throw("input too large", false);

  lua_createtable(ls, narr, nrec);
}
#else
# define aclua_createtable(narr, nrec)  lua_createtable(ls, (int)(narr), (int)(nrec))
#endif


/**
  # Load properties from global table
*/

static
void
convert_property(Aircloak__JobResponsePB__Property **prop)
{
  /* ensure it is a table */
  if (!lua_istable(ls, -1))
    throw("bad property", false);

  /* allocate memory for the property result */
  if ((*prop = calloc(sizeof(Aircloak__JobResponsePB__Property), 1)) == NULL)
    throw("out of memory when converting property", false);
  aircloak__job_response_pb__property__init(*prop);

  /* get the label */
  lua_getfield(ls, -1, "label");
  if (!lua_isstring(ls, -1))
    throw("bad property label", false);
  prop[0]->label = get_lua_string(-1);
  lua_pop(ls, 1);

  /* get the value */
  lua_getfield(ls, -1, "value");
  if (!lua_isstring(ls, -1))
    throw("bad property value", false);
  prop[0]->value = get_lua_string(-1);
  lua_pop(ls, 1);
}

static
Aircloak__JobResponsePB__Property **
convert_property_table(size_t *props_size)
{
  size_t                             i;
  Aircloak__JobResponsePB__Property  **props;


  /* drop all the environment */
  lua_getglobal(ls, "ac_properties");

  /* if it does not exists we do not report anything */
  if (lua_isnil(ls, -1)) {
    lua_pop(ls, 1);
    props_size = 0;
    return NULL;
  }

  /* generate the corresponding execution response property list from the table */
  if (!lua_istable(ls, -1))
    throw("bad property table", false);

  /* stupid counting, I think otherwise we do not get the list of non-sequence tables */
  *props_size = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    (*props_size)++;
    lua_pop(ls, 1);
  }

  /* allocate array of properties */
  if (SIZE_MAX / sizeof(Aircloak__JobResponsePB__Property *) < *props_size)
    throw("too many properties", false);
  if ((props = calloc(sizeof(Aircloak__JobResponsePB__Property *), *props_size)) == NULL)
    throw("out of memory when converting property table", false);

  /* save all properties */
  i = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    convert_property(props + i);
    lua_pop(ls, 1); // remove value, keep key for next iteration
    i++;
  }

  return props;
}


/**
  # Extract table actions from the global Lua table
*/

static
void
convert_insert_action_row(Aircloak__InsertDataPB__Row **row)
{
  size_t  i = 0;

  /* ensure it is a table */
  if (!lua_istable(ls, -1))
    throw("not a table row", false);

  /* allocate memory for the property result */
  if ((*row = calloc(sizeof(Aircloak__InsertDataPB__Row), 1)) == NULL)
    throw("out of memory when inserting row", false);
  aircloak__insert_data_pb__row__init(*row);

  /* stupid counting, I think otherwise we do not get the list of non-sequence tables */
  row[0]->n_fields = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    row[0]->n_fields++;
    lua_pop(ls, 1);
  }

  /* allocate enough memory */
  if (SIZE_MAX / sizeof(Aircloak__InsertDataPB__Field *) < row[0]->n_fields)
    throw("not enough space for all fields", false);
  if ((row[0]->fields = calloc(sizeof(Aircloak__InsertDataPB__Field *), row[0]->n_fields)) == NULL)
    throw("out of memory when inserting row", false);

  /* get all rows */
  i = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    /* allocate memory for field and initialize */
    if ((row[0]->fields[i] = calloc(sizeof(Aircloak__InsertDataPB__Field), 1)) == NULL)
      throw("out of memory when inserting row", false);
    aircloak__insert_data_pb__field__init(row[0]->fields[i]);

    /* set field data */
    int value_type = lua_type(ls, -1);
    if (value_type == LUA_TNUMBER) {
      row[0]->fields[i]->has_number = true;
      row[0]->fields[i]->number = lua_tonumber(ls, -1);
    } else if (value_type == LUA_TSTRING) {
      row[0]->fields[i]->string = get_lua_string(-1);
    } else if (value_type == LUA_TBOOLEAN) {
      row[0]->fields[i]->has_boolean = true;
      row[0]->fields[i]->boolean = lua_toboolean(ls, -1);
    } else
      throw("bad field data", false);
    lua_pop(ls, 1);

    /* set field name */
    if (!lua_isstring(ls, -1))
      throw("bad field name", false);
    row[0]->fields[i]->name = get_lua_string(-1);

    /* go to next position */
    i++;
  }
}

static
void
convert_insert_action_rows(Aircloak__InsertDataPB *ia)
{
  size_t  i = 0;

  /* stupid counting, I think otherwise we do not get the list of non-sequence tables */
  ia->n_rows = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    ia->n_rows++;
    lua_pop(ls, 1);
  }

  /* allocate enough memory */
  if (SIZE_MAX / sizeof(Aircloak__InsertDataPB__Row *) < ia->n_rows)
    throw("too many rows", false);
  if ((ia->rows = calloc(sizeof(Aircloak__InsertDataPB__Row *), ia->n_rows)) == NULL)
    throw("out of memory when inserting rows", false);

  /* get all rows */
  i = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    lua_pop(ls, 1);
    convert_insert_action_row(ia->rows + i);
    i++;
  }
}

static
size_t
convert_insert_action(Aircloak__InsertDataPB **ia)
{
  /* allocate memory for the property result */
  if ((*ia = calloc(sizeof(Aircloak__InsertDataPB), 1)) == NULL)
    throw("out of memory when converting insert action", false);
  aircloak__insert_data_pb__init(*ia);

  /* get the label */
  if (!lua_isstring(ls, -2))
    throw("bad table name", false);
  ia[0]->table = get_lua_string(-2);

  /* get the rows */
  if (lua_isnil(ls, -1))
    ia[0]->n_rows = 0;
  else if (lua_istable(ls, -1))
    convert_insert_action_rows(ia[0]);
  else
    throw("bad row", false);

  return ia[0]->n_rows;
}

static
Aircloak__InsertDataPB **
convert_insert_action_table(size_t *ia_size)
{
  size_t                 i;
  Aircloak__InsertDataPB  **ias;


  /* drop all the environment */
  lua_getglobal(ls, "ac_insert_actions");

  /* if it does not exists we do not change anything */
  if (lua_isnil(ls, -1)) {
    lua_pop(ls, 1);
    *ia_size = 0;
    return NULL;
  }

  /* generate the corresponding execution response property list from the table */
  if (!lua_istable(ls, -1))
    throw("bad insert action table", false);

  /* stupid counting, I think otherwise we do not get the list of non-sequence tables */
  *ia_size = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    (*ia_size)++;
    lua_pop(ls, 1);
  }

  /* allocate array of properties */
  if (SIZE_MAX / sizeof(Aircloak__InsertDataPB *) < *ia_size)
    throw("too many insert actions", false);
  if ((ias = calloc(sizeof(Aircloak__InsertDataPB *), *ia_size)) == NULL)
    throw("out of memory when converting insert actions", false);

  /* save all insert actions */
  i = 0;
  lua_pushnil(ls);
  while (lua_next(ls, -2) != 0) {
    if (convert_insert_action(ias + i) > 100)
      throw("too many inserted rows", false);
    lua_pop(ls, 1);
    i++;
  }

  return ias;
}


/**
  # Running the given code and create response
*/

static
void
run_lua_code(char *code, size_t code_length)
{
  /* run the code */
  run_with_reduced_state(code, code_length);

  /* create the execution response and send it back */
  aircloak__job_response_pb__init(&job_response);
  job_response.status = AIRCLOAK__JOB_RESPONSE_PB__STATUS__OK;
  job_response.properties = convert_property_table(&job_response.n_properties);
  job_response.table_insert_actions = convert_insert_action_table(&job_response.n_table_insert_actions);

  /* serialize the accumulator and store it to response */
  /* Note: the accumulator must be fetched from the job environment */
  lua_getglobal(ls, "job_environment");
  lua_getglobal(ls, "serialize");
  lua_getfield(ls, -2, "accumulator");
  do_lua_pcall(1, 1);
  if (lua_isstring(ls, -1))
    job_response.accumulator = get_lua_string(-1);
  else
    job_response.accumulator = NULL;

  Aircloak__JobOutputMessagePB output_message;
  aircloak__job_output_message_pb__init(&output_message);
  output_message.job_response = &job_response;
  write_job_output_message(&output_message);
}


/**
  # The full run cycle
*/

static
void
run_next_job(void)
{
  job_create_message = read_job_input_message();
  if (!job_create_message->job_request)
    throw("not a job request", false);

  create_lua_state();

  /* deserialize the accumulator */
  lua_getglobal(ls, "deserialize");
  if (job_create_message->job_request->accumulator)
    lua_pushstring(ls, job_create_message->job_request->accumulator);
  else
    lua_pushnil(ls);
  do_lua_pcall(1, 1);
  lua_setglobal(ls, "accumulator");

  if (job_create_message->job_request->has_task_time)
    lua_pushnumber(ls, job_create_message->job_request->task_time);
  else
    lua_pushnil(ls);
  lua_setglobal(ls, "task_time");

  run_lua_code((char *)job_create_message->job_request->code.data, job_create_message->job_request->code.len);

  sandbox_release_resources();
}


/**
  # Initializing limits such that the sandbox is not wasting memory
*/

static const rlim_t  limit_address_space = 256 * 1024 * 1024;  /* 256 MB */

static
void
set_limits(void)
{
  const struct rlimit  rlimit = { limit_address_space, limit_address_space };
  if (setrlimit(RLIMIT_AS, &rlimit))
    abort();
}

/**
  # Bridge for generic calls into Erlang function from lua interpreter.
*/
static
int
external_call(lua_State *L)
{
  free_external_call_request();
  if ((external_call_request = calloc(sizeof(Aircloak__FunctionCallPB), 1)) == NULL)
    throw("out of memory when doing external call", false);
  aircloak__function_call_pb__init(external_call_request);

  // First arg must be a string that represents the function name.
  if (lua_gettop(L) < 1 || !lua_isstring(L, 1)) {
    throw("invalid arguments for external call", false);
  }

  // Get function name.
  external_call_request->name = get_lua_string(1);
  lua_remove(L, 1); // remove function name from the stack

  // Get all remaining args (variable).
  acstack_to_datapb(L);

  // Populate and send output message
  Aircloak__JobOutputMessagePB output_message;
  aircloak__job_output_message_pb__init(&output_message);
  output_message.function_call = external_call_request;

  write_job_output_message(&output_message);
  free_external_call_request();

  // Wait for return message
  job_reply_message = read_job_input_message();
  if (!job_reply_message->data) {
    throw("invalid input message while waiting for the external call response", false);
  }

  // Push response to lua stack
  acpush_datapb(L, job_reply_message->data);
  free_job_reply_message();

  return 1; // We always return a single value to lua.
}


/**
  # Pushes an integer to the stack as a table field.
*/
static void push_table_int_field(lua_State *L, const char *key, int value) {
  lua_pushinteger(L, value);
  lua_setfield(L, -2, key);
}


/**
  # Simplified port of os.date from lua os package. Receives an epoch time (integer or string),
    and returns a table with datetime parts. It is assumed that time is in UTC.
*/
static
int
to_date(lua_State *L) {
  if (lua_isnoneornil(L, 1) || !lua_isnumber(L, 1)) {
    lua_pushnil(L);
    return 1;
  }
  time_t t = (time_t) lua_tonumber(L, 1);
  struct tm *stm = gmtime(&t);

  if (stm == NULL) {
    // invalid date
    lua_pushnil(L);
    return 1;
  }

  lua_createtable(L, 0, 8);
  push_table_int_field(L, "sec", stm->tm_sec);
  push_table_int_field(L, "min", stm->tm_min);
  push_table_int_field(L, "hour", stm->tm_hour);
  push_table_int_field(L, "day", stm->tm_mday);
  push_table_int_field(L, "month", stm->tm_mon+1);
  push_table_int_field(L, "year", stm->tm_year+1900);
  push_table_int_field(L, "wday", stm->tm_wday+1);
  push_table_int_field(L, "yday", stm->tm_yday+1);
  return 1;
}


/**
  # Bridge for getting the next batch of rows from a table.
*/

static
int
get_next_batch(lua_State *L)
{
  free_get_next_batch_request();
  if ((get_next_batch_request = calloc(sizeof(Aircloak__GetNextBatchPB), 1)) == NULL)
    throw("out of memory while reading user table", false);
  aircloak__get_next_batch_pb__init(get_next_batch_request);

  // verify function arguments: table_name - string, reset_stream - boolean
  if (lua_gettop(L) != 2 || !lua_isstring(L, 1) || !lua_isboolean(L, 2)) {
    throw("invalid arguments for get batch call", false);
  }

  // get table name.
  get_next_batch_request->table_name = get_lua_string(1);
  // get reset stream flag.
  get_next_batch_request->reset_stream = lua_toboolean(L, 2);

  // populate and send output message
  Aircloak__JobOutputMessagePB output_message;
  aircloak__job_output_message_pb__init(&output_message);
  output_message.get_next_batch = get_next_batch_request;

  write_job_output_message(&output_message);
  free_get_next_batch_request();

  // wait for return message
  job_reply_message = read_job_input_message();
  if (!job_reply_message->table_data) {
    throw("user table read operation failed", false);
  }

  // create batch object on stack
  Aircloak__JobInputMessagePB** batch_object =
      (Aircloak__JobInputMessagePB**)lua_newuserdata(L, sizeof(Aircloak__JobInputMessagePB*));
  if (batch_object == NULL) {
    throw("out of memory when creating batch object", false);
  }
  *batch_object = job_reply_message;
  // set batch object metatable
  luaL_getmetatable(L, "batch_mt");
  lua_setmetatable(L, -2);

  // push complete flag
  lua_pushboolean(L, job_reply_message->table_data->complete);

  // prevent double deletion of object
  job_reply_message = NULL;

  return 2; // return batch object and complete flag
}


/**
  # Indexer for batch objects
*/

static
int
get_batch_row(lua_State* L)
{
  // verify function arguments: batch - userdata, index - integer
  if (lua_gettop(L) != 2 || !lua_isuserdata(L, 1) || !lua_isnumber(L, 2)) {
    throw("invalid arguments for get batch row call", false);
  }

  // get parameters
  Aircloak__JobInputMessagePB* batch_object = *(Aircloak__JobInputMessagePB**)lua_touserdata(L, 1);
  if (batch_object == NULL || batch_object->table_data == NULL) {
    throw("invalid batch object supplied", false);
  }
  Aircloak__TableDataPB* table = batch_object->table_data;
  size_t index = (size_t)lua_tointeger(L, 2);

  // check index
  if (index < 1 || index > table->n_rows) {
    lua_pushnil(L); // invalid index
  } else {
    // assemble required row
    Aircloak__TableDataPB__Row* row = table->rows[index-1];
    if (row->n_fields != table->n_columns) {
        throw("invalid batch format", false);
    }

    // create a new table for the given row
    aclua_createtable(0, row->n_fields);

    // add all fields
    for(size_t i=0; i<row->n_fields; i++) {
      if (row->fields[i]->has_number)
        lua_pushnumber(L, row->fields[i]->number);
      else if (row->fields[i]->string != NULL)
        lua_pushstring(L, row->fields[i]->string);
      else if (row->fields[i]->has_boolean)
        lua_pushboolean(L, row->fields[i]->boolean);
      else
        lua_pushnil(L);
      lua_setfield(L, -2, table->columns[i]);
    }
  }

  return 1; // return row
}


/**
  # Finalizer for batch objects
*/

static
int
free_batch(lua_State* L)
{
  // verify function arguments: batch - userdata
  if (lua_gettop(L) != 1 || !lua_isuserdata(L, 1)) {
    throw("invalid arguments for free batch call", false);
  }

  job_reply_message = *(Aircloak__JobInputMessagePB**)lua_touserdata(L, 1);
  free_job_reply_message();
  return 0;
}


/**
  # The `main()` loop
*/

int
main(void)
{
  set_limits();
  read_helpers_lua();

  while (true) {
    sandbox_error_run(run_next_job);
  }

  return EXIT_SUCCESS;  /* this is here just for the compiler */
}
