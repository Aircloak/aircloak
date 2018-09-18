.PHONY: all compile deps clean test dialyze docs lint format check-format

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

data_quality:
	mix data_quality

clean:
	mix clean --deps

test:
	mix test

dialyze:
	mix dialyze_retry

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted
