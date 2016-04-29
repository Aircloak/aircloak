SPIDER_MONKEY_VERSION = 38.2.1.rc0
SPIDER_MONKEY_BZ2 = mozjs-$(SPIDER_MONKEY_VERSION).tar.bz2
SPIDER_MONKEY_SOURCE = https://people.mozilla.org/~sstangl/$(SPIDER_MONKEY_BZ2)
SPIDER_MONKEY_LOCAL_SOURCE = mozjs/$(SPIDER_MONKEY_VERSION)
SPIDER_MONKEY_JS_SRC = $(SPIDER_MONKEY_LOCAL_SOURCE)/js/src
OUT_DIR = bin
OUT_BIN = jsport
SOURCES = $(wildcard *.cpp)
HEADERS = $(wildcard *.h)

ifeq ($(shell uname),Darwin)
MOZJS_LIB = $(SPIDER_MONKEY_JS_SRC)/dist/lib/libmozjs-.dylib
else
MOZJS_LIB = $(SPIDER_MONKEY_JS_SRC)/dist/lib/libmozjs-.so
endif


.PHONY: clean

all: $(OUT_DIR)/$(OUT_BIN)

clean:
	rm -rf $(OUT_DIR)/*

$(OUT_DIR)/$(OUT_BIN): $(MOZJS_LIB) $(SOURCES) $(HEADERS)
	mkdir -p $(OUT_DIR)
	cp -rp $(MOZJS_LIB) $(OUT_DIR)
	$(CXX) -std=c++11 -Wall -Werror -fPIC -O3 \
		-I$(SPIDER_MONKEY_JS_SRC)/dist/include -L$(SPIDER_MONKEY_JS_SRC)/dist/lib \
		-Wno-invalid-offsetof \
		-o $(OUT_DIR)/$(OUT_BIN) $(SOURCES) -lmozjs- -lz -lpthread -ldl -Wl,-rpath '-Wl,$$ORIGIN'

$(MOZJS_LIB): $(SPIDER_MONKEY_LOCAL_SOURCE)
	cd $(SPIDER_MONKEY_JS_SRC) && ./configure && $(MAKE)
	strip -x $(MOZJS_LIB)

$(SPIDER_MONKEY_LOCAL_SOURCE): mozjs/download/$(SPIDER_MONKEY_BZ2)
	bzcat mozjs/download/$(SPIDER_MONKEY_BZ2) > mozjs/download/mozjs.tar
	mkdir -p $(SPIDER_MONKEY_LOCAL_SOURCE)
	tar --strip-components=1 -xf mozjs/download/mozjs.tar -C $(SPIDER_MONKEY_LOCAL_SOURCE)

mozjs/download/$(SPIDER_MONKEY_BZ2):
	mkdir -p mozjs/download
	wget --output-document=mozjs/download/$(SPIDER_MONKEY_BZ2) $(SPIDER_MONKEY_SOURCE)
	touch mozjs/download/$(SPIDER_MONKEY_BZ2)
