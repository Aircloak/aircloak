SHELL=/bin/bash -o pipefail

.PHONY: sandbox deps docs proper proper-std proper-extended test clean all compile regenerate_db dialyze lint release

all: deps compile

compile: config/dev.exs
	mix compile

sandbox: config/dev.exs
	mix compile.sandbox

deps: config/dev.exs
	mix deps.get

clean: config/dev.exs
	mix clean --deps

start: config/dev.exs
	iex -S mix

test:
	mix test.standard

regenerate_db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./regenerate_db.sh

docs: config/dev.exs
	mix docs.all

# a proper run may take some time...
# Note: proper-full is temporarily removed because of big changes in the code.
# Once full tests are fixed, need to include it again.
proper: proper-std proper-extended

proper-std: config/dev.exs
	mix proper --level simple

proper-extended: config/dev.exs
	mix proper --level extended

dialyze: config/dev.exs
	mix dialyze_retry

lint: config/dev.exs
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

release:
	rm -rf rel/cloak
	MIX_ENV=prod mix do compile, release --no-confirm-missing

check_dependent_apps:
	mix check_dependent_apps

config/dev.exs:
	cp config/dev.exs.example config/dev.exs
