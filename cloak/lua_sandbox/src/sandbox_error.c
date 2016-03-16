/**
  % Error handling
  % Aircloak
  % May 2014
*/

#include "sandbox_error.h"

#include "job_communicator.h"

#include <setjmp.h>
#include <stdlib.h>


/**
  Global variables
*/

Aircloak__JobInputMessagePB   *job_create_message = NULL;
Aircloak__JobResponsePB  job_response;
Aircloak__FunctionCallPB *external_call_request = NULL;
Aircloak__GetNextBatchPB *get_next_batch_request = NULL;
Aircloak__JobInputMessagePB *job_reply_message = NULL;
lua_State                *ls = NULL;
char *dynamic_error_message = NULL;


/**
  # Running with error handler
*/

static jmp_buf     error_env;
static const char  * volatile error_message = NULL;
static bool        release_message = false;

static
void
free_job_response(void);

void
sandbox_error_run(void (*worker)(void))
{
  /* first clear the job_response to prevent old data in that global variable
   * from leaking over and generating double free(3)s */
  aircloak__job_response_pb__init(&job_response);
  release_message = false;
  error_message = NULL;

  if (setjmp(error_env) == 0) {
    worker();
    return;
  }

  /* release all memory except for the error message */
  sandbox_release_resources();

  /* generate error message */
  aircloak__job_response_pb__init(&job_response);
  job_response.status = AIRCLOAK__JOB_RESPONSE_PB__STATUS__ERROR;
  job_response.error = (char *)error_message;  /* we know what we are doing here... */

  Aircloak__JobOutputMessagePB output_message;
  aircloak__job_output_message_pb__init(&output_message);
  output_message.job_response = &job_response;
  write_job_output_message(&output_message);

  free(dynamic_error_message);
  dynamic_error_message = NULL;
  free_job_response();
  free_external_call_request();
  free_get_next_batch_request();
  free_job_reply_message();
}


/**
  # Releasing resources
*/

static
void
free_job_response(void)
{
  size_t  i, j, k;


  if (release_message)
    free(job_response.error);

  if (job_response.properties) {
    for (i=0; i<job_response.n_properties; i++) {
      if (job_response.properties[i]) {
        free(job_response.properties[i]->label);
        free(job_response.properties[i]->value);
        free(job_response.properties[i]);
      }
    }
    free(job_response.properties);
  }

  if (job_response.table_insert_actions) {
    for (i=0; i<job_response.n_table_insert_actions; i++) {
      if (job_response.table_insert_actions[i]) {
        free(job_response.table_insert_actions[i]->table);
        if (job_response.table_insert_actions[i]->rows) {
          for (j=0; j<job_response.table_insert_actions[i]->n_rows; j++) {
            if (job_response.table_insert_actions[i]->rows[j]) {
              if (job_response.table_insert_actions[i]->rows[j]->fields) {
                for (k=0; k<job_response.table_insert_actions[i]->rows[j]->n_fields; k++) {
                  if (job_response.table_insert_actions[i]->rows[j]->fields[k]) {
                    free(job_response.table_insert_actions[i]->rows[j]->fields[k]->name);
                    free(job_response.table_insert_actions[i]->rows[j]->fields[k]->string);
                    free(job_response.table_insert_actions[i]->rows[j]->fields[k]);
                  }
                }
                free(job_response.table_insert_actions[i]->rows[j]->fields);
              }
              free(job_response.table_insert_actions[i]->rows[j]);
            }
          }
          free(job_response.table_insert_actions[i]->rows);
        }
        free(job_response.table_insert_actions[i]);
      }
    }
    free(job_response.table_insert_actions);
  }
}

static
void
free_datapb_recursive(Aircloak__DataPB *data) {
  if (data == NULL)
    return;

  free(data->string_val);

  if (data->arr_val) {
    for (size_t i = 0;i < data->arr_val->n_elements; i++) {
      free_datapb_recursive(data->arr_val->elements[i]);
    }

    free(data->arr_val);
  }
  free(data);
}

void
free_external_call_request(){
  if (external_call_request == NULL)
    return;
  free(external_call_request->name);
  if (external_call_request->args) {
    for (size_t i = 0; i < external_call_request->args->n_elements; i++) {
      free_datapb_recursive(external_call_request->args->elements[i]);
    }
    free(external_call_request->args->elements);
    free(external_call_request->args);
  }
  free(external_call_request);
  external_call_request = NULL;
}

void
free_get_next_batch_request(){
  if (get_next_batch_request == NULL)
    return;
  if(get_next_batch_request->table_name)
    free(get_next_batch_request->table_name);
  free(get_next_batch_request);
  get_next_batch_request = NULL;
}

void
free_job_reply_message() {
  if (job_reply_message == NULL)
    return;

  aircloak__job_input_message_pb__free_unpacked(job_reply_message, NULL);
  job_reply_message = NULL;
}

void
sandbox_release_resources(void)
{
  /* cleanup everything */
  if (job_create_message)
    aircloak__job_input_message_pb__free_unpacked(job_create_message, NULL);
  free_job_response();
  free_external_call_request();
  free_get_next_batch_request();
  free_job_reply_message();
  if (ls)
    lua_close(ls);

  /* reset standard values */
  job_create_message = NULL;
  ls = NULL;
}


/**
  # Throwing an error
*/

void
throw(const char *message, bool rel_message)
{
  error_message = message;
  release_message = rel_message;
  longjmp(error_env, 1);
}
