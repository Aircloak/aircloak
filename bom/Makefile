.PHONY: all compile deps clean test dialyze docs lint format check-format

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

clean:
	mix clean --deps

test:
	mix test

dialyze:
	mix dialyze_retry

docs:
	mix docs

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted
