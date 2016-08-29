.PHONY: all compile deps clean start test dialyze docs lint

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

clean:
	mix clean --deps

start:
	iex -S mix start

test:
	mix test

dialyze:
	mix dialyze_retry

docs:
	mix docs

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint
