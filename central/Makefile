.PHONY: all compile deps clean start start2 start3 test recreate-db migrate rollback dialyze docs release release-local lint mocha format check-format

all: deps compile

compile:
	mix compile

deps:
	mix deps.get
	cd assets && yarn install && cd ..

clean:
	mix clean --deps

start:
	iex -S mix phx.server

test: mocha
	mix test.standard

recreate-db:
	mix rollback --all
	mix seed

migrate:
	mix migrate
	MIX_ENV=test mix migrate

rollback:
	mix rollback
	MIX_ENV=test mix rollback

dialyze:
	mix dialyze_retry

docs:
	mix docs

release:
	NODE_ENV=production MIX_ENV=prod mix do compile, site_release

release-local:
	NODE_ENV=production MIX_ENV=prod mix do compile, site_release --env=local --no-tar

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

mocha:
	cd assets && npm test

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted
