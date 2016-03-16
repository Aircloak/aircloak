/**
  % JobCommunicator
  % Aircloak
  % May 2014

  This file contains the implementation of the job reader and writer facilities.
*/

#include "job_communicator.h"

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

#if SIZE_MAX < UINT32_MAX
# error "we do not support systems with SIZE_MAX < UINT32_MAX"
#endif


/**
  # Reading a job request

  - `read_min_size()`:  auxiliary function to fill a buffer of a fixed size but read a given minimum amount of
    data.

  - `read_fixed_size()`: auxiliary function to fill a buffer of a fixed size completely.
*/

static
ssize_t
read_min_size(unsigned char *buffer, size_t len, size_t min)
{
  ssize_t  res;
  size_t   pos = 0;


  /* do nothing if we have everything we want */
  if (min == 0)
    return 0;

  do {
    res = read(STDIN_FILENO, buffer + pos, len - pos);
    if (res > 0)
      pos += (size_t)res;
  } while ((res > 0 && min > pos) || (res < 0 && (errno == EINTR || errno == EAGAIN)));

  if (res == 0)
    exit(EXIT_SUCCESS);  /* cloak-core closed port */
  if (res < 0)
    abort();  /* communication broken: force a reset */

  return pos; /* pos is the number of bytes read */
}

static inline
void
read_fixed_size(unsigned char *buffer, size_t len)
{
  (void)read_min_size(buffer, len, len);
}

static const size_t INITIAL_BUFFER_SIZE = 4096;

Aircloak__JobInputMessagePB *
read_job_input_message(void)
{
  unsigned char                 size_buf[INITIAL_BUFFER_SIZE], *buffer;
  ssize_t                       bytes_read;
  size_t                        buf_size;
  Aircloak__JobInputMessagePB   *req;


  /* first read the given buffer size */
  bytes_read = read_min_size(size_buf, INITIAL_BUFFER_SIZE, 4);
  buf_size = ((size_t)size_buf[0] << 24UL) + ((size_t)size_buf[1] << 16UL) + ((size_t)size_buf[2] << 8UL) +
      (size_t)size_buf[3];

  /* now allocate, copy contents from initial buffer, and read the buffer */
  if ((buffer = malloc(buf_size)) == NULL)
    abort();
  if (bytes_read > 4)
    memcpy(buffer, size_buf + 4, bytes_read - 4);
  read_fixed_size(buffer + bytes_read - 4, buf_size - bytes_read + 4);

  /* decode the job request and return it */
  if ((req = aircloak__job_input_message_pb__unpack(NULL, buf_size, buffer)) == NULL)
    abort(); /* we cannot communicate with cloak-core;  force a reset */
  free(buffer);
  return req;
}


/**
  # Write execution response

  - `write_fixed_size()`:  write the complete given buffer back to standard output.
*/

static
void
write_fixed_size(uint8_t *buffer, size_t len)
{
  ssize_t  res;
  size_t   pos = 0;


  do {
    res = write(STDOUT_FILENO, buffer + pos, len - pos);
    if (res > 0)
      pos += (size_t)res;
  } while ((res > 0 && (len - pos) > 0) || (res < 0 && (errno == EINTR || errno == EAGAIN)));

  if (res == 0)
    exit(EXIT_SUCCESS);  /* cloak-core closed port */
  if (res < 0)
    abort(); /* this should generate a core-dump if enabled for inspection with a debugger */
}

void
write_job_output_message(Aircloak__JobOutputMessagePB *rep)
{
  size_t   buf_size;
  uint8_t  *buffer;


  buf_size = aircloak__job_output_message_pb__get_packed_size(rep);
  if (SIZE_MAX - buf_size < 4)
    abort(); /* overflow: should never happen except on strange platforms (like oldish DOS) */
  if ((buffer = malloc(buf_size + 4)) == NULL)
    abort();
  buffer[0] = buf_size >> 24;
  buffer[1] = buf_size >> 16;
  buffer[2] = buf_size >> 8;
  buffer[3] = buf_size;
  aircloak__job_output_message_pb__pack(rep, buffer + 4);
  write_fixed_size(buffer, buf_size + 4);
  free(buffer);
}
