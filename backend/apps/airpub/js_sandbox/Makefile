SOURCES = $(wildcard *.cpp)
HEADERS = $(wildcard *.h)
OUT_DIR = ../../../priv/js/bin
OUT_BIN = jsport
JS_LIB  = mozjs-
LIBS    = -l$(JS_LIB) -lz -lpthread -ldl

all: $(OUT_DIR) $(OUT_DIR)/$(OUT_BIN) $(OUT_DIR)/lib$(JS_LIB).so

$(OUT_DIR):
	mkdir $@

$(OUT_DIR)/$(OUT_BIN): $(SOURCES) $(HEADERS)
	g++ -std=c++11 -Wall -Werror -fPIC -O3 -I./js_engine/include -L./js_engine/bin -o $(OUT_DIR)/$(OUT_BIN) $(LIBS) -Wl,-rpath '-Wl,$$ORIGIN' -Wno-invalid-offsetof $(SOURCES)

$(OUT_DIR)/lib$(JS_LIB).so:
	ln ../../../apps/airpub/js_sandbox/js_engine/bin/lib$(JS_LIB).so $@

clean:
	rm -f $(OUT_DIR)/$(OUT_BIN)
	rm -f $(OUT_DIR)/lib$(JS_LIB).so
