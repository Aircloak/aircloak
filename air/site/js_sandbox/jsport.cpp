#include "jsapi.h"
#include "comm.h"
#include "codec.h"

#include <stdint.h>
#include <stdlib.h>
#include <sys/resource.h>

// The global buffer to store the last error message.
// Notice: we can't return error messages bigger than the size of this buffer.
char g_error[8 * 1024];

// The error reporter callback.
static void report_error(JSContext* cx, const char* message, JSErrorReport* report)
{
  snprintf (g_error, sizeof(g_error), "[%s:%u] %s",
          report->filename ? report->filename : "<unknown>",
          (uint32_t)report->lineno,
          message);
}

// Reports the result of the last action back to the calling process.
static void report_result(JSContext* cx, bool ok, const JS::HandleValue& rval)
{
  OutMessage message = {}; // message to be sent back
  message.body.reserve(16 * 1024); // avoid small allocations

  if (ok)
  {
    message.success = true;
    if (!write_value(&message.body, cx, rval))
    {
      ok = false;
      message.body.resize(0);
      strcpy(g_error, "Operation returned an invalid value.");
    }
  }
  if (!ok)
  {
    message.success = false;
    append_string(&message.body, g_error, (uint32_t)strlen(g_error));
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
  while (size > 0)
  {
    JS::RootedValue value(cx);

    uint32_t read = unpack_value(cx, &value, buffer, size);
    buffer += read;
    size -= read;

    args->append(value);
  }
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

int main(int argc, const char* argv[])
{
  JS_Init();

  JSRuntime* rt = JS_NewRuntime(8L * 1024 * 1024); // run GC in 8 MB steps.
  if (!rt)
    return 1;

  JSContext* cx = JS_NewContext(rt, 8192);
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
