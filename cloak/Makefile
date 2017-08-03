SHELL=/bin/bash -o pipefail

.PHONY: sandbox deps docs proper proper-std proper-extended test clean all compile
.PHONY: recreate-db dialyze lint release release-local perftest perftest-setup

all: deps compile

compile:
	mix compile
	mix compile.app --force

sandbox:
	mix compile.sandbox

deps:
	mix deps.get

clean:
	mix clean --deps

start: compile
	iex -S mix

test:
	mix test

test_all:
	TRAVIS=true mix test --include exclude_in_dev

recreate-db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./datagen/recreate_db.sh

recreate-db-more-data: recreate-db
	./datagen/add_more_test_data.sh

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
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	mix release

release-local:
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	mix release --env=local --no-tar

perftest:
	MIX_ENV=test mix run perftest/main.exs "${QUERY}"

perftest-setup:
	MIX_ENV=test mix run perftest/setup.exs ${SIZE}
