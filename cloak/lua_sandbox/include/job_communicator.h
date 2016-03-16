/**
  % JobCommunicator
  % Aircloak
  % May 2014
*/

#ifndef JOB_COMMUNICATOR_H
#define JOB_COMMUNICATOR_H

#include "sandbox.pb-c.h"

/**
  `read_job_input_message()`:  Read the next job input message and return the corresponding handle for it.
*/

Aircloak__JobInputMessagePB *
read_job_input_message(void);


/**
  `write_job_output_message()`:  Send the given job message back to `cloak-core`.
*/

void
write_job_output_message(Aircloak__JobOutputMessagePB *rep);

#endif /* !defined(JOB_COMMUNICATOR_H) */
