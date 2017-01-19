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

test_all:
	TRAVIS=true mix test --include exclude_in_dev

recreate_db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./recreate_db.sh

recreate_db_more_data: recreate_db
	./add_more_test_data.sh

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
	mix release

release-local:
	mix release --env=local --no-tar

perftest:
	MIX_ENV=test mix run perftest/main.exs "${QUERY}"

perftest-setup:
	MIX_ENV=test mix run perftest/setup.exs ${SIZE}
