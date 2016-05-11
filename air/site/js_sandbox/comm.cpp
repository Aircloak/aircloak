#include "comm.h"

#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>

#if defined(__APPLE__)
  #include <libkern/OSByteOrder.h>
  #define bswap_16 OSSwapInt16
  #define bswap_32 OSSwapInt32
  #define bswap_64 OSSwapInt64
#else
  #include <byteswap.h>
#endif


// Internal method for reading a fixed amount of data from the input stream.
static void read_data(void* buffer, uint32_t size)
{
  while(size > 0)
  {
    ssize_t result = read(STDIN_FILENO, buffer, size);

    if (result > 0)
    {
      buffer = (uint8_t*)buffer + (uint32_t)result;
      size -= (uint32_t)result;
    }
    else
    {
      if (result < 0 && (errno == EINTR || errno == EAGAIN))
        continue; // transient error, retry
      else
        exit(EXIT_FAILURE);
    }
  }
}

// Internal method for writing data to the output stream.
static void write_data(const void* buffer, uint32_t size)
{
  while(size > 0)
  {
    ssize_t result = write(STDOUT_FILENO, buffer, size);

    if (result > 0)
    {
      buffer = (const uint8_t*)buffer + (uint32_t)result;
      size -= (uint32_t)result;
    }
    else
    {
      if (result < 0 && (errno == EINTR || errno == EAGAIN))
        continue; // transient error, retry
      else
        exit(EXIT_FAILURE);
    }
  }
}

void read_message(InMessage* message)
{
  struct __attribute__((__packed__))
  {
    uint32_t packetSize; // this is in network order (big-endian)
    Action action;
    uint8_t nameSize;
  } header = {};

  read_data(&header, sizeof(header));
  header.packetSize = bswap_32(header.packetSize);

  message->action = header.action;

  message->name.resize(header.nameSize + 1);
  read_data(message->name.data(), header.nameSize);
  message->name[header.nameSize] = 0;

  uint32_t bodySize = header.packetSize - (sizeof(header) - sizeof(header.packetSize) + header.nameSize);
  message->body.resize(bodySize);
  read_data(message->body.data(), bodySize);
}

void write_message(const OutMessage& message)
{
  struct __attribute__((__packed__))
  {
    uint32_t packetSize; // this is in network order (big-endian)
    bool success; // result of the performed action
  } header = {0, message.success};
  header.packetSize = bswap_32(message.body.size() + sizeof(header) - sizeof(header.packetSize));

  write_data(&header, sizeof(header));

  write_data(message.body.data(), message.body.size());
}
