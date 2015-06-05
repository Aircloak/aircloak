.PHONY: sandbox deps doc test test-eunit clean distclean rel relclean stage app all compile

all: deps compile sandbox

compile: src/air_cloak_conf.erl
	./rebar compile

src/air_cloak_conf.erl: rel/files/sys.config deps/cloak/rel/files/app.config
	./generate_cloak_conf.escript

sandbox: copy-sandbox-makefile-local
	$(MAKE) -C deps/cloak sandbox
	mkdir -p priv
	cp -rp deps/cloak/priv/* ./priv

deps: deps/.make

deps/.make:
	rm -rf ./deps
	$(MAKE) deps-dev
	touch deps/.make

deps-dev:
	./rebar -C rebar.config.lock get-deps
	(cd deps/cloak && git submodule update --init --recursive)

copy-sandbox-makefile-local:
	if [ -f sandbox_makefile.local ]; then cp -rp sandbox_makefile.local deps/cloak/lua_sandbox/Makefile.local; fi;

update-deps:
	rm -f rebar.config.lock
	rm -rf ./deps
	./rebar get-deps
	./rebar compile
	./rebar lock-deps
	touch deps/.make

clean:
	./rebar clean
	rm -rf test-gen
	rm -rf priv/sandbox

distclean: clean relclean
	./rebar delete-deps

apps/air:
	mkdir -p apps/air
	(cd apps/air && ln -s ../../ebin ebin && ln -s ../../deps/cloak/include include)

rel: all apps/air
	(cd rel && ../rebar generate skip_deps=true)

start: app
	./run_local_node.sh 1

relclean:
	rm -rf rel/air

stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/air/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/air/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/air/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/air/lib;)

app: src/air_cloak_conf.erl sandbox
	./rebar compile skip_deps=true

test: app test-eunit

test/sys.config: rel/files/sys.config
	./make_test_config.sh

test-eunit: test/sys.config
	mkdir -p ./.eunit
	cp -rp priv ./.eunit
	./rebar skip_deps=true eunit

regenerate_db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./regenerate_db.sh

# Notice that the rebar-edoc-plugin fails if not both the doc or ebin
# directories are present
doc:
	cp -rp deps/cloak/include/*_pb.hrl ./include/
	mkdir -p doc
	mkdir -p ebin
	./rebar skip_deps=true doc


## DIALYZER

.PHONY: rebuild-plt dialyzer typer

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
BASE_PLT=$(shell echo "$$REBAR_PLT_DIR/dialyzer.plt")

DEPS_DIALYZER_LIST=$(shell ls deps | grep -v rebar)

$(DEPSOLVER_PLT):
	@if [ -z "$$REBAR_PLT_DIR" ]; then echo "no kerl?  panic!"; exit 1; fi
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt -c ${DEPS_DIALYZER_LIST:%=deps/%/ebin} ebin/*.beam

rebuild-plt:
	@if [ -z "$$REBAR_PLT_DIR" ]; then echo "no kerl?  panic!"; exit 1; fi
	@echo "WARNING: this will stop make with an error!  dialyzer outputs warnings!"
	@echo "WARNING: protobuf output included in PLT!"
	@echo "WARNING: you need to have compiled dependencies to generate the PLT."
	@#echo "WARNING: this will take some time.  Building starts in 10s, press ^C if you do not want this."
	@#sleep 10
	rm -f $(DEPSOLVER_PLT)
	$(MAKE) $(DEPSOLVER_PLT)

dialyzer: $(DEPSOLVER_PLT)
	@if [ ! -f $(BASE_PLT) ]; then dialyzer --build_plt --apps erts kernel stdlib --output_plt $(BASE_PLT); fi
	@if [ -z "$$REBAR_PLT_DIR" ]; then echo "no kerl?  panic!"; exit 1; fi
	@echo "WARNING: protobuf output included in PLT!"
	dialyzer \
				--no_native -Werror_handling -Wunderspecs -Wrace_conditions \
				-pa deps/lager/ebin -I deps -I deps/cloak/include \
				-I src --src src --plts $(BASE_PLT) $(DEPSOLVER_PLT) | \
			grep -E -v -f ./dialyzer.ignore-warnings

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
