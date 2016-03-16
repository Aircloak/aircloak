/**
  % LuaDataPb
  % Aircloak
  % May 2014
*/

#ifndef LUA_DATA_PB
#define LUA_DATA_PB

#include "sandbox.pb-c.h"

/**
  `acstack_to_datapb()`:  Transfers all lua stack variables to data protobuff structure.
*/
void
acstack_to_datapb(lua_State *L);

/**
  `acpush_datapb()`:  Recursively pushes protobuff data to lua stack.
*/
void
acpush_datapb(lua_State *L, const Aircloak__DataPB *data);

#endif /* !defined(LUA_DATA_PB) */
