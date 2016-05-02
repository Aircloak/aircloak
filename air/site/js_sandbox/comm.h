#pragma once

#include <stdint.h>
#include <vector>

// The data types supported by the sandbox.
// TODO: add support for arrays and maps.
enum class DataType : uint8_t {Undefined, Null, Boolean, Int32, Double, String};
// The actions that can be executed by the sandbox.
enum class Action : uint8_t {Stop, Evaluate, Call};

// Generic message for executing a sandbox action.
struct InMessage
{
  Action action;
  std::vector<char> name;
  std::vector<uint8_t> body;
};

// Generic message for returning the result of an action.
struct OutMessage
{
  bool success;
  DataType type;
  uint32_t size;
  const uint8_t* data;
};

// Reads a new message from the controlling process.
void read_message(InMessage* message);
// Responds to the previously received message.
void write_message(const OutMessage& message);
