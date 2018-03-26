SHELL=/bin/bash -o pipefail

.PHONY: deps docs test clean all compile dialyze lint format check-format

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

clean:
	mix clean --deps

test:
	mix test.standard

test-cover:
	mix test.cover

docs:
	mix docs

# a proper run may take some time...
# Note: proper-full is temporarily removed because of big changes in the code.
# Once full tests are fixed, need to include it again.
proper: proper-std proper-extended

proper-std:
	mix proper --level simple

proper-extended:
	mix proper --level extended

dialyze:
	mix dialyze_retry

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

release:
	rm -rf rel/cloak
	MIX_ENV=prod mix do compile, release --no-confirm-missing

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted
