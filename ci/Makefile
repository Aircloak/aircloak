SHELL=/bin/bash -o pipefail

.PHONY: deps test clean all compile dialyze lint docs release format check-format

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

clean:
	mix clean --deps

start: compile
	iex -S mix

test:
	mix test

docs:
	mix docs

dialyze:
	mix dialyze_retry

release: deps
	mix release

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted
