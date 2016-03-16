/**
  % Lua DataPB
  % Aircloak
  % May 2014

  Implements conversion functions for external calls from lua to Erlang.
*/

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

#include "lua_includes.h"

#include "sandbox_error.h"

#include "lua_datapb.h"

char *
get_lua_string(int stack_pos);

/**
  `aclua_table_size()`:  Computes the size of a table by walking each element. Preserves the stack, and
                         works well with positive and negative references.
*/
static
int
aclua_table_size(lua_State *L, int stack_pos)
{
    int count = 0;

    // Push another reference to the table on top of the stack (so we know where it is, and this function
    // can work for negative and positive stack references)
    lua_pushvalue(L, stack_pos);        // stack: -1 => table

    lua_pushnil(L);                     // stack: -1 => nil; -2 => table
    while (lua_next(L, -2))
    {                                   // stack: -1 => value; -2 => key; -3 => table
      lua_pop(L, 1);                    // stack: -1 => key; -2 => table
      count++;
    }

    // stack: -1 => table (when lua_next returns 0 it pops the key but does not push anything.)
    // pop table to make the stack the same
    lua_pop(L, 1);

    return count;
}


/**
  `aclua_to_datapb()`:  Recursively converts a single lua value to protobuff data.
*/
static
void
aclua_to_datapb(lua_State *L, int stack_pos, Aircloak__DataPB *destination) {
  aircloak__data_pb__init(destination);

  if (lua_isnumber(L, stack_pos)) {
    destination->double_val = lua_tonumber(L, stack_pos);
    destination->has_double_val = true;
  }
  else if (lua_isboolean(L, stack_pos)) {
    destination->bool_val = lua_toboolean(L, stack_pos);
    destination->has_bool_val = true;
  }
  else if (lua_isstring(L, stack_pos)) {
    destination->string_val = get_lua_string(stack_pos);
  }
  else if (lua_istable(L, stack_pos)) {
    int table_size = aclua_table_size(L, stack_pos);

    Aircloak__DataPBArray *args_arr;
    if ((args_arr = calloc(sizeof(Aircloak__DataPBArray), 1)) == NULL)
      throw("out of memory", false);
    aircloak__data_pbarray__init(args_arr);

    if ((args_arr->elements = calloc(sizeof(Aircloak__DataPB *), table_size)) == NULL)
      throw("out of memory", false);
    args_arr->n_elements = table_size;

    destination->arr_val = args_arr;

    // Same iteration as in aclua_table_size
    int pos = 0;
    lua_pushvalue(L, stack_pos);
    lua_pushnil(L);
    while (lua_next(L, -2))
    {
      if ((args_arr->elements[pos] = calloc(sizeof(Aircloak__DataPB), 1)) == NULL)
        throw("out of memory", false);

      // on -1 we have the value. Key is completely ignored.
      aclua_to_datapb(L, -1, args_arr->elements[pos]);

      lua_pop(L, 1);
      pos++;
    }

    // Pop the table to preserve the stack
    lua_pop(L, 1);
  }
  else if (!lua_isnil(L, stack_pos)) {
    throw("invalid data type", false);
  }
}


void
acstack_to_datapb(lua_State *L) {
  if (!external_call_request){
    throw("external call request not initialized", false);
  }

  int n = lua_gettop(L);
  if (n == 0) {
    return;
  }

  if ((external_call_request->args = calloc(sizeof(Aircloak__DataPBArray), 1)) == NULL)
    throw("out of memory", false);
  aircloak__data_pbarray__init(external_call_request->args);

  if ((external_call_request->args->elements = calloc(sizeof(Aircloak__DataPB *), n)) == NULL)
    throw("out of memory", false);

  external_call_request->args->n_elements = n;

  for (int i = 0; i < n; i++) {
    if ((external_call_request->args->elements[i] = calloc(sizeof(Aircloak__DataPB), 1)) == NULL)
      throw("out of memory", false);

    aclua_to_datapb(L, i + 1, external_call_request->args->elements[i]);
  }
}


void
acpush_datapb(lua_State *L, const Aircloak__DataPB *data) {
  if (data->has_bool_val) {
    lua_pushboolean(L, data->bool_val);
  }
  else if (data->has_double_val) {
    lua_pushnumber(L, data->double_val);
  }
  else if (data->string_val != NULL) {
    lua_pushstring(L, data->string_val);
  }
  else if (data->arr_val) {
    lua_newtable(L);
    for (size_t i = 0;i < data->arr_val->n_elements; i++) {
      lua_pushnumber(L, i+1);
      acpush_datapb(L, data->arr_val->elements[i]);
      lua_settable(L, -3);
    }
  }
  else {
    lua_pushnil(L);
  }
}
