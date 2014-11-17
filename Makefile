.PHONY: deps doc clean distclean rel relclean app all compile

all: deps compile

compile:
	./rebar compile

deps: deps/.make

deps/.make: rebar.config.lock
	rm -rf deps
	./rebar -C rebar.config.lock get-deps
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

apps/airpub:
	mkdir -p apps/airpub
	(cd apps/airpub && ln -s ../../ebin ebin)

rel: all apps/airpub
	rm -rf rel/airpub/ # we want a fully new release each time
	./rebar generate
	# make process path consistent for easy monitoring
	mv rel/airpub/erts-* rel/airpub/erts-current
	echo "current 1" > rel/airpub/releases/start_erl.data

start: rel
	cp test/test.html rel/airpub/
	rel/airpub/bin/airpub console

relclean:
	rm -rf rel/airpub

doc:
	mkdir -p doc
	./rebar skip_deps=true doc

ERLFLAGS= -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib crypto inets ssl

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	rm -f $(DEPS_PLT)
	dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT) compile
	@echo Running dialyzer
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin
