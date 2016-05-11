#include <vector>
#include "jsapi.h"
#include "comm.h"

#define VERIFY(expr) if (!(expr)) exit(EXIT_FAILURE)

// Adds a value to a raw output buffer.
static void append_data(std::vector<uint8_t>* buffer, DataType type, const uint8_t* data, uint32_t size)
{
  uint32_t index = buffer->size();
  buffer->resize(index + 1 + size); // make room
  uint8_t* target = buffer->data() + index;

  *target++ = (uint8_t)type; // append type
  memcpy(target, data, size); // append data
}

// Adds a string to a raw output buffer.
void append_string(std::vector<uint8_t>* buffer, const char* string, uint32_t size)
{
  uint32_t index = buffer->size();
  buffer->resize(index + 1 + sizeof(size) + size); // make room
  uint8_t* target = buffer->data() + index;

  *target++ = (uint8_t)DataType::String; // append type
  memcpy(target, &size, sizeof(size)); // append size
  target += sizeof(size);
  memcpy(target, string, size); // append string
  target += size;
}

// Packs the specified JavaScript value into the raw output. Returns false if the value is invalid.
bool write_value(std::vector<uint8_t>* buffer, JSContext *cx, const JS::HandleValue& rval)
{
  if (rval.isUndefined())
  {
    buffer->push_back((uint8_t)DataType::Undefined);
  }
  else if (rval.isNull())
  {
    buffer->push_back((uint8_t)DataType::Null);
  }
  else if (rval.isBoolean())
  {
    bool value = rval.toBoolean();
    buffer->push_back((uint8_t)DataType::Boolean);
    buffer->push_back((uint8_t)value);
  }
  else if (rval.isInt32())
  {
    int32_t value = rval.toInt32();
    append_data(buffer, DataType::Int32, (uint8_t*)&value, (uint32_t)sizeof(value));
  }
  else if (rval.isDouble())
  {
    double value = rval.toDouble();
    append_data(buffer, DataType::Double, (uint8_t*)&value, (uint32_t)sizeof(value));
  }
  else if (rval.isString())
  {
    JS::RootedString str(cx, rval.toString());
    JSAutoByteString bytes; // container to hold UTF-8 string data
    bytes.encodeUtf8(cx, str);
    append_string(buffer, bytes.ptr(), (uint32_t)bytes.length());
  }
  else if (JS_IsArrayObject(cx, rval))
  {
    JS::RootedObject object(cx, &rval.toObject());
    uint32_t length = 0;
    if (!JS_GetArrayLength(cx, object, &length))
      return false;
    append_data(buffer, DataType::Array, (uint8_t*)&length, (uint32_t)sizeof(length));
    for (uint32_t index = 0; index < length; index++)
    {
      JS::RootedValue element(cx);
      if (!JS_GetElement(cx, object, index, &element))
        return false;
      if (!write_value(buffer, cx, element))
        return false;
    }
  }
  else if (rval.isObject())
  {
    JS::RootedObject object(cx, &rval.toObject());
    JS::AutoIdArray properties(cx, JS_Enumerate(cx, object));
    if (!properties) // check the returned value from JS_Enumerate
      return false;

    uint32_t length = properties.length();
    append_data(buffer, DataType::Map, (uint8_t*)&length, (uint32_t)sizeof(length));

    for (uint32_t index = 0; index < length; index++)
    {
      JS::RootedId propertyId(cx, properties[index]);
      // retrieve and write property
      JS::RootedValue property(cx);
      if (!JS_IdToValue(cx, propertyId, &property))
        return false;
      if (!write_value(buffer, cx, property))
        return false;
      // retrieve and write element
      JS::RootedValue element(cx);
      if (!JS_GetPropertyById(cx, object, propertyId, &element))
        return false;
      if (!write_value(buffer, cx, element))
        return false;
    }
  }
  else
  {
    return false; //unknown value type
  }
  return true;
}

// Unpacks a values from the raw input buffer. Returns the amount of bytes consumed from the buffer.
uint32_t unpack_value(JSContext* cx, JS::MutableHandleValue rval, const uint8_t* buffer, uint32_t size)
{
  uint32_t initial_size = size; // save initial size

  // read the value type
  VERIFY (size > 0);
  DataType type = (DataType)buffer[0];
  buffer++;
  size--;
  // unpack value depending on the type
  switch(type)
  {
    case DataType::Undefined:
      rval.set(JS::UndefinedValue());
      break;

    case DataType::Null:
      rval.set(JS::NullValue());
      break;

    case DataType::Boolean:
      {
        VERIFY (size >= sizeof(bool));

        bool value = *(bool*)buffer;
        buffer += sizeof(bool);
        size -= sizeof(bool);

        rval.set(JS::BooleanValue(value));
      }
      break;

    case DataType::Int32:
      {
        VERIFY (size >= sizeof(int32_t));

        int32_t value = *(int32_t*)buffer;
        buffer += sizeof(int32_t);
        size -= sizeof(int32_t);

        rval.set(JS::Int32Value(value));
      }
      break;

    case DataType::Double:
      {
        VERIFY (size >= sizeof(double));

        double value = *(double*)buffer;
        buffer += sizeof(double);
        size -= sizeof(double);

        rval.set(JS::DoubleValue(value));
      }
      break;

    case DataType::String:
      {
        VERIFY (size >= sizeof(uint32_t));

        uint32_t length = *(uint32_t*)buffer;
        buffer += sizeof(uint32_t);
        size -= sizeof(uint32_t);

        VERIFY (size >= length);

        JS::RootedString value(cx, JS_NewStringCopyN(cx, (const char*)buffer, length));
        VERIFY (value);

        buffer += length;
        size -= length;

        rval.set(JS::StringValue(value));
      }
      break;

    case DataType::Array:
      {
        // read array length
        VERIFY (size >= sizeof(uint32_t));
        uint32_t length = *(uint32_t*)buffer;
        buffer += sizeof(uint32_t);
        size -= sizeof(uint32_t);

        JS::RootedObject value(cx, JS_NewArrayObject(cx, length)); // create empty array
        VERIFY (value);

        // unpack and return 'length' count items
        for(uint32_t index = 0; index < length; index++)
        {
            JS::RootedValue element(cx);

            uint32_t read = unpack_value(cx, &element, buffer, size);
            buffer += read;
            size -= read;

            VERIFY (JS_SetElement(cx, value, index, element));
        }

        rval.set(JS::ObjectValue(*value));
      }
      break;

      case DataType::Map:
        {
          // read map length
          VERIFY (size >= sizeof(uint32_t));
          uint32_t length = *(uint32_t*)buffer;
          buffer += sizeof(uint32_t);
          size -= sizeof(uint32_t);

          JS::RootedObject value(cx, JS_NewPlainObject(cx)); // create empty map
          VERIFY (value);

          // unpack and return 'length' count {key, value} pairs
          while(length-- > 0)
          {
              uint32_t read = 0;

              JS::RootedValue property(cx);
              read = unpack_value(cx, &property, buffer, size);
              buffer += read;
              size -= read;

              JS::RootedValue element(cx);
              read = unpack_value(cx, &element, buffer, size);
              buffer += read;
              size -= read;

              JS::RootedId propertyId(cx);
              VERIFY (JS_ValueToId(cx, property, &propertyId));
              VERIFY (JS_SetPropertyById(cx, value, propertyId, element));
          }

          rval.set(JS::ObjectValue(*value));
        }
        break;

    default:
      exit(EXIT_FAILURE);
      break;
  }

  return initial_size - size;
}
