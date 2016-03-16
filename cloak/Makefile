SHELL=/bin/bash -o pipefail

.PHONY: sandbox deps doc proper proper-std proper-full proper-extended test test-eunit clean distclean rel relclean stage app all compile

all: deps compile sandbox

compile:
	./rebar compile

sandbox:
	$(MAKE) -C lua_sandbox depend
	$(MAKE) -C lua_sandbox
	mkdir -p priv
	cp lua_sandbox/obj/lua_sandbox priv/sandbox
	cp lua_sandbox/src/helpers.lua priv/helpers.lua
	cp lua_sandbox/src/serialization.lua priv/serialization.lua

deps: deps/.make

deps/.make: rebar.config.lock
	rm -rf ./deps
	./rebar -C rebar.config.lock get-deps
# We need to compile the deps here so that we have all the rebar plugins built before doing the actual compile.
# Otherwise, the first compilation would be done without the plugins active, leading to inconsistent results.
# This is an issue, for example, for erlcron, which depends on the rebar_vsn_plugin.
	./rebar compile
	touch deps/.make

update-deps:
	rm -f rebar.config.lock
	rm -rf ./deps
	./rebar get-deps
	./rebar compile
	./rebar lock-deps
	touch deps/.make

clean:
	./rebar clean
	rm -rf .proper undefined .data
	rm -rf test-gen
	rm -f rebar.proper
	rm -f priv/sandbox priv/helpers.lua priv/serialization.lua
	$(MAKE) -C lua_sandbox clean

distclean: clean devclean relclean
	./rebar delete-deps

apps/cloak:
	mkdir -p apps/cloak
	(cd apps/cloak && ln -s ../../ebin ebin && ln -s ../../include include)

rel: all apps/cloak
	rm -f rel/cloak/.erlang.cookie # delete the pre-created cookie before the build
	./rebar generate
	chmod 400 rel/cloak/.erlang.cookie

install: rel
ifndef BASE_IMAGE_PATH
	@echo "BASE_IMAGE_PATH is not set. Usage: 'make install BASE_IMAGE_PATH=\"/path/to/base/image\"'"
else
	cp -R -d ./rel/cloak $(BASE_IMAGE_PATH)/aircloak/
endif

start: app .data/app.config
	./run_local_node.sh 1

# Useful for creating extra nodes for local cluster testing
start2: app .data/app.config
	./run_local_node.sh 2
start3: app .data/app.config
	./run_local_node.sh 3

.data/app.config: test/app.config
	mkdir -p .data
	cat test/app.config | sed 's,.proper/data,.data/1,' | sed 's/cloaktest1/cloak/' | \
            sed 's/{lager_console_backend, error}/{lager_console_backend, info}/' > .data/app.config

relclean:
	rm -rf rel/cloak

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/cloak/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/cloak/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/cloak/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/cloak/lib;)

app:
	./rebar compile skip_deps=true

test: test-eunit proper-std

test-eunit: sandbox test/app.config
	mkdir -p ./.eunit
	cp -rp priv ./.eunit
	./rebar skip_deps=true eunit

regenerate_db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./regenerate_db.sh

# Notice that the rebar-edoc-plugin fails if not both the doc or ebin
# directories are present
doc:
	mkdir -p doc
	mkdir -p ebin
	./rebar skip_deps=true doc

# a proper run may take some time...
# Note: proper-full is temporarily removed because of big changes in the code.
# Once full tests are fixed, need to include it again.
proper: proper-std

# Runs proper tests with remote node output printed to the console.
proper-verbose: export PROPER_NODE_STDOUT=true
proper-verbose: proper

proper-std: rebar.proper test/app.config
	mkdir -p .proper
	cp -f ebin/*_pb.beam .proper
	cp ebin/cloak.app .proper
	rm -rf .proper/data
	mkdir -p .proper/data
	env RUNNING_PROPER=true ./rebar.proper skip_deps=true proper

rebar.proper: rebar.config rebar.proper.template
	test/tools/generate-prefix
	cp test-gen/prefix rebar.proper
	cat rebar.proper.template >> rebar.proper
	chmod u+x rebar.proper

test/app.config: rel/files/app.config test/tools/generate-app.config
	test/tools/generate-app.config

proper-full:
	env RUNNING_PROPER=true test/tools/run-full
	sh proper_prepare_db.sh || exit 1


# extended proper tests
proper-extended:
	@echo "WARNING: THIS MAY TAKE A VERY VERY LONG TIME AND REQUIRES >1GB OF MEMORY FOR ERLANG"
	@echo "first we run the standard proper targets"
	$(MAKE) proper
	env RUNNING_PROPER=true test/tools/run-extended
	sh proper_prepare_db.sh || exit 1


## DIALYZER

.PHONY: rebuild-plt dialyzer typer

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
BASE_PLT=$(shell echo "$$REBAR_PLT_DIR/dialyzer.plt")

DEPS_DIALYZER_LIST=$(shell ls deps | grep -v rebar)

$(DEPSOLVER_PLT):
	@set -e -o pipefail
	@if [ -z "$$REBAR_PLT_DIR" ]; then echo "no kerl?  panic!"; exit 1; fi
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt -c ${DEPS_DIALYZER_LIST:%=deps/%/ebin} ebin/*_pb.beam

rebuild-plt:
	@if [ ! -f $(BASE_PLT) ]; then dialyzer --build_plt --apps erts kernel stdlib --output_plt $(BASE_PLT); fi
	@if [ -z "$$REBAR_PLT_DIR" ]; then echo "no kerl?  panic!"; exit 1; fi
	@echo "WARNING: this will stop make with an error!  dialyzer outputs warnings!"
	@echo "WARNING: protobuf output included in PLT!"
	@echo "WARNING: you need to have compiled dependencies to generate the PLT."
	@#echo "WARNING: this will take some time.  Building starts in 10s, press ^C if you do not want this."
	@#sleep 10
	rm -f $(DEPSOLVER_PLT)
	$(MAKE) $(DEPSOLVER_PLT)

dialyzer: $(DEPSOLVER_PLT)
	@if [ -z "$$REBAR_PLT_DIR" ]; then echo "no kerl?  panic!"; exit 1; fi
	@echo "WARNING: protobuf output included in PLT!"
	dialyzer \
				--no_native -Werror_handling -Wrace_conditions \
				-pa deps/lager/ebin -pa deps/proper/ebin -I deps \
				-I src -I include -DDIALYZER --src src --plts $(BASE_PLT) $(DEPSOLVER_PLT) | \
			sed -f ./dialyzer.ignore-unknown-functions

typer: $(DEPSOLVER_PLT)
	@if [ -z "$$REBAR_PLT_DIR" ]; then echo "no kerl?  panic!"; exit 1; fi
	@echo "WARNING: protobuf output included in PLT!"
	typer -pa deps/lager/ebin -I deps -I include -I src -r ./src --plt $(BASE_PLT) --plt $(DEPSOLVER_PLT)


##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel
DEVNODES ?= 4

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	  $(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev
