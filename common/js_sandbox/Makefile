SHELL=/bin/bash -o pipefail

.PHONY: deps docs test clean all compile dialyze lint

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
	mix docs.all

dialyze:
	mix dialyze_retry

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

check_dependent_apps:
	mix check_dependent_apps
