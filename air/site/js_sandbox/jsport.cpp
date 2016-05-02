#include "jsapi.h"
#include "comm.h"

#include <stdint.h>
#include <stdlib.h>
#include <sys/resource.h>


// The global buffer to store the last error message.
// Notice: we can't return error messages bigger than the size of this buffer.
char g_error[8 * 1024];

// The error reporter callback.
static void report_error(JSContext *cx, const char *message, JSErrorReport *report)
{
  snprintf (g_error, sizeof(g_error), "[%s:%u] %s",
          report->filename ? report->filename : "<unknown>",
          (uint32_t)report->lineno,
          message);
}

// Reports the result of the last action back to the calling process.
static void report_result(JSContext *cx, bool ok, const JS::HandleValue& rval)
{
  OutMessage message = {}; // message to be sent back
  union
  {
    bool b;
    int32_t i;
    double d;
  } value; // container to hold bool, int or double data
  JSAutoByteString bytes; // container to hold string data

  if (ok)
  {
    message.success = true;

    if (rval.isUndefined())
    {
      message.type = DataType::Undefined;
    }
    else if (rval.isNull())
    {
      message.type = DataType::Null;
    }
    else if (rval.isBoolean())
    {
      message.type = DataType::Boolean;
      value.b = rval.toBoolean();
      message.size = sizeof(value.b);
      message.data = (uint8_t*)&value.b;
    }
    else if (rval.isInt32())
    {
      message.type = DataType::Int32;
      value.i = rval.toInt32();
      message.size = sizeof(value.i);
      message.data = (uint8_t*)&value.i;
    }
    else if (rval.isDouble())
    {
      message.type = DataType::Double;
      value.d = rval.toDouble();
      message.size = sizeof(value.d);
      message.data = (uint8_t*)&value.d;
    }
    else if (rval.isString())
    {
      JS::RootedString str(cx, rval.toString());
      bytes.encodeUtf8(cx, str);
      message.type = DataType::String;
      message.size = (uint32_t)bytes.length();
      message.data = (uint8_t*)bytes.ptr();
    }
    else
    {
      ok = false;
      strcpy(g_error, "Operation returned an unsupported value type.");
    }
  }
  if (!ok)
  {
    message.success = false;
    message.type = DataType::String;
    message.size = (uint32_t)strlen(g_error);
    message.data = (uint8_t*)g_error;
  }

  write_message(message);
}

// Executes a script in the sandbox and returns the result of the operation.
static void evaluate_script(JSContext* cx, const JS::HandleObject& global,
                     const char* name, const uint8_t* content, uint32_t contentSize)
{
    JS::RootedValue rval(cx);
    JS::CompileOptions opts(cx);
    opts.setFileAndLine(name, 1);
    bool ok = JS::Evaluate(cx, global, opts, (const char*)content, contentSize, &rval);
    report_result(cx, ok, rval);
}

// Calls a function in the sandbox and returns the result of the operation.
static void call_function(JSContext* cx, const JS::HandleObject& global,
                     const char* name, const JS::HandleValueArray& args)
{
  JS::RootedValue rval(cx);
  bool ok = JS_CallFunctionName(cx, global, name, args, &rval);
  report_result(cx, ok, rval);
}

// Unpacks the arguments for a function call out of a raw buffer received from the calling process.
static void unpack_args(JSContext* cx, JS::AutoValueVector* args, const uint8_t* buffer, uint32_t size)
{
  if(size == 0)
    return;

  DataType type = (DataType)buffer[0];
  buffer++;
  size--;

  switch(type)
  {
    case DataType::Undefined:
      args->append(JS::UndefinedValue());
      break;

    case DataType::Null:
      args->append(JS::NullValue());
      break;

    case DataType::Boolean:
      {
        if (size < sizeof(bool))
          exit(EXIT_FAILURE);

        bool value = *(bool*)buffer;
        buffer += sizeof(bool);
        size -= sizeof(bool);

        args->append(JS::BooleanValue(value));
      }
      break;

    case DataType::Int32:
      {
        if (size < sizeof(int32_t))
          exit(EXIT_FAILURE);

        int32_t value = *(int32_t*)buffer;
        buffer += sizeof(int32_t);
        size -= sizeof(int32_t);

        args->append(JS::Int32Value(value));
      }
      break;

    case DataType::Double:
      {
        if (size < sizeof(double))
          exit(EXIT_FAILURE);

        double value = *(double*)buffer;
        buffer += sizeof(double);
        size -= sizeof(double);

        args->append(JS::DoubleValue(value));
      }
      break;

    case DataType::String:
      {
        if (size < sizeof(uint32_t))
          exit(EXIT_FAILURE);

        uint32_t length = *(uint32_t*)buffer;
        buffer += sizeof(uint32_t);
        size -= sizeof(uint32_t);

        if (size < length)
          exit(EXIT_FAILURE);

        JS::RootedString value(cx, JS_NewStringCopyN(cx, (const char*)buffer, length));
        buffer += length;
        size -= length;

        args->append(JS::StringValue(value));
      }
      break;

    default:
      exit(EXIT_FAILURE);
      break;
  }

  unpack_args(cx, args, buffer, size);
}

// This is the main message loop of the program. It will wait for a message to arrive from
//  the controlling process, execute the specified action and then respond back with the result.
static void message_loop(JSContext* cx, const JS::HandleObject& global)
{
  while(true)
  {
    InMessage message;
    read_message(&message);

    switch(message.action)
    {
      case Action::Stop:
        return;

      case Action::Evaluate:
        evaluate_script(cx, global, message.name.data(), message.body.data(), (uint32_t)message.body.size());
        break;

      case Action::Call:
        {
          JS::AutoValueVector args(cx);
          unpack_args(cx, &args, message.body.data(), (uint32_t)message.body.size());
          call_function(cx, global, message.name.data(), args);
        }
        break;

      default:
        exit(EXIT_FAILURE);
        return;
    }
  }
}

// Sets the maximum memory that can be consumed by the sandbox process.
void set_memory_limit(uint64_t memoryLimit)
{
  const rlimit rlimit = {memoryLimit, memoryLimit};
  if (setrlimit(RLIMIT_AS, &rlimit))
    exit(EXIT_FAILURE);
}

int main(int argc, const char *argv[])
{
  uint64_t memoryLimit = 256; // 256 MB by default
  if (argc == 2)
  {
    memoryLimit = atoi(argv[1]);
    if (memoryLimit == 0)
      exit(EXIT_FAILURE);
  }
  set_memory_limit(memoryLimit * 1024 * 1024);

  JS_Init();

  JSRuntime *rt = JS_NewRuntime(8L * 1024 * 1024); // run GC in 8 MB steps.
  if (!rt)
    return 1;

  JSContext *cx = JS_NewContext(rt, 8192);
  if (!cx)
    return 1;

  JS_SetErrorReporter(rt, report_error);

  { // Scope for our various stack objects (JSAutoRequest, RootedObject), so they all go
    // out of scope before we JS_DestroyContext.

    JSAutoRequest ar(cx);

    static JSClass global_class = {"global", JSCLASS_GLOBAL_FLAGS}; // The class of the global object.
    JS::RootedObject global(cx, JS_NewGlobalObject(cx, &global_class, nullptr, JS::FireOnNewGlobalHook));
    if (!global)
        return 1;

    // Scope for JSAutoCompartment
    JSAutoCompartment ac(cx, global);
    JS_InitStandardClasses(cx, global);

    message_loop(cx, global);
  }

  JS_DestroyContext(cx);
  JS_DestroyRuntime(rt);
  JS_ShutDown();

  return 0;
}
