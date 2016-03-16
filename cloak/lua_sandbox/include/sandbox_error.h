/**
  % Error handling
  % Aircloak
  % May 2014

  This module contains facilities for the error handling.  Error handling is based upon the approach, that we
  pack any allocated structures into global variables such that a cleanup function can automatically clean out
  every problematic piece of code.  The error handling itself is implemented via `setjmp(3)`/`longjmp(3)`.  We
  reimplement the Lua error handler to perform the corresponding `longjmp(3)`.
*/

#ifndef SANDBOX_ERROR_H
#define SANDBOX_ERROR_H

#include "sandbox.pb-c.h"

#include <stdbool.h>

#include "lua_includes.h"


/**
  # Global variables
*/

extern Aircloak__JobInputMessagePB   *job_create_message;
extern Aircloak__JobResponsePB  job_response;
extern Aircloak__FunctionCallPB *external_call_request;
extern Aircloak__GetNextBatchPB *get_next_batch_request;
extern Aircloak__JobInputMessagePB *job_reply_message;
extern lua_State                *ls;
extern char *dynamic_error_message;


/**
  # Running with error handler
*/

void
sandbox_error_run(void (*worker)(void));


/**
  # Releasing resources
*/

void
sandbox_release_resources(void);

/**
  `free_external_call_request()`:  Releases memory of external call request
*/
void
free_external_call_request();

/**
  `free_get_next_batch_request()`:  Releases memory of get next batch request
*/
void
free_get_next_batch_request();

/**
  `free_job_reply_message()`:  Releases memory of the job reply message
*/
void
free_job_reply_message();


/**
  # Generating an error

  The memory of `message` is `free(3)`ed if `rel_message` is set to `true`.
*/

void
throw(const char *message, bool rel_message);


#endif /* !defined(SANDBOX_ERROR_H) */
