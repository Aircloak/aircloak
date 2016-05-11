#pragma once

// Functions for packing / unpacking JavaScript values into / from raw buffers.

// Adds a string to a raw output buffer.
void append_string(std::vector<uint8_t>* buffer, const char* string, uint32_t size);

// Packs the specified JavaScript value into the raw output. Returns false if the value is invalid.
bool write_value(std::vector<uint8_t>* buffer, JSContext *cx, const JS::HandleValue& rval);

// Unpacks a values from the raw input buffer. Returns the amount of bytes consumed from the buffer.
uint32_t unpack_value(JSContext* cx, JS::MutableHandleValue rval, const uint8_t* buffer, uint32_t size);
