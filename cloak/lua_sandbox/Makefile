.PHONY: all clean clean-deps depend create_lua_includes

.SECONDARY:

OBJ_DIR=obj

-include Makefile.local

CC?=gcc
LUALIB?=lua5.1
LUAINCPATH?=lua5.1

CPPFLAGS+=-I include -I src-gen
CFLAGS+=-Wall -Wextra -Werror -std=c99 -pedantic
LDFLAGS+=-l$(LUALIB) -lprotobuf-c

GEN_SRC=src-gen
FULL_GEN_SRC=$(PWD)/$(GEN_SRC)

PROTOC=protoc-c
PROTOC_FLAGS=--proto_path=../proto --c_out=$(GEN_SRC)

SOURCES_C=$(shell ls src/*.c)
PROTOBUFS=sandbox
PROTOBUFS_C=${PROTOBUFS:%=$(GEN_SRC)/%.pb-c.c}
PROTOBUFS_H=${PROTOBUFS:%=$(GEN_SRC)/%.pb-c.h}
MODULES=${SOURCES_C:src/%.c=obj/%.o} ${PROTOBUFS_C:$(GEN_SRC)/%.c=obj/%.o}

all: $(OBJ_DIR)/lua_sandbox

depend: $(PROTOBUFS_H) create_lua_includes
	rm -f Makefile.depend
	for source in $(SOURCES_C) $(PROTOBUFS_H); do printf obj/ >> Makefile.depend; $(CC) -E $(CPPFLAGS) -MM -MF Makefile.depend.temp $$source > /dev/null 2>&1 ; cat Makefile.depend.temp >> Makefile.depend; done
	rm -f Makefile.depend.temp

-include Makefile.depend

clean:
	rm -rf $(GEN_SRC) $(OBJ_DIR) Makefile.depend

clean-deps: clean
	rm -rf messages


# Protobuf handling
$(GEN_SRC)/%.pb-c.h: ../proto/%.proto $(GEN_SRC)/.keep
	$(PROTOC) $(PROTOC_FLAGS) $<

$(GEN_SRC)/.keep:
	mkdir -p $(GEN_SRC)
	touch $@


# C Code handling
$(OBJ_DIR)/%.o: src/%.c $(OBJ_DIR)/.keep
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

$(OBJ_DIR)/%.o: $(GEN_SRC)/%.c $(OBJ_DIR)/.keep
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

$(OBJ_DIR)/.keep:
	mkdir -p $(OBJ_DIR)
	touch $@

$(OBJ_DIR)/lua_sandbox: $(MODULES)
	$(CC) -o $@ $(MODULES) $(LDFLAGS)


# Generate correct includes for Lua
create_lua_includes: $(GEN_SRC)/.keep
	echo "#include <$(LUAINCPATH)/lua.h>" > $(GEN_SRC)/lua_includes.h
	echo "#include <$(LUAINCPATH)/lualib.h>" >> $(GEN_SRC)/lua_includes.h
	echo "#include <$(LUAINCPATH)/lauxlib.h>" >> $(GEN_SRC)/lua_includes.h
