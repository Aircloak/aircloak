SHELL=/bin/bash -o pipefail

.PHONY: sandbox deps docs proper proper-std proper-extended test clean all compile
.PHONY: regenerate_db dialyze lint release release-local perftest perftest-setup

all: deps compile

compile:
	mix compile

sandbox:
	mix compile.sandbox

deps:
	mix deps.get

clean:
	mix clean --deps

start:
	iex -S mix

test:
	mix test

regenerate_db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./regenerate_db.sh

docs:
	mix docs

# a proper run may take some time...
# Note: proper-full is temporarily removed because of big changes in the code.
# Once full tests are fixed, need to include it again.
proper: proper-std proper-extended

proper-std:
	MIX_ENV=test mix proper --level simple

proper-extended:
	MIX_ENV=test mix proper --level extended

dialyze:
	mix dialyze_retry

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

release:
	rm -rf rel/cloak
	MIX_ENV=prod mix do compile, release --no-confirm-missing

release-local: release
	cp rel/cloak/lib/cloak-0.1.0/priv/config/dev.json rel/cloak/lib/cloak-0.1.0/priv/config/config.json

check_dependent_apps:
	mix check_dependent_apps

perftest:
	MIX_ENV=test mix run perftest/main.exs "${QUERY}"

perftest-setup:
	MIX_ENV=test mix run perftest/setup.exs ${SIZE}
