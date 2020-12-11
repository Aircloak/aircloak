.PHONY: all compile deps clean start test test-migrations recreate-db migrate rollback dialyze docs
.PHONY: release release-local lint mocha format check-format

all: deps compile

compile:
	mix compile
	mix compile.app --force

deps:
	mix deps.get
	cd assets && yarn install && cd ..

clean:
	mix clean --deps

start: compile
	iex -S mix phx.server

test: mocha test-migrations
	mix test

test-migrations:
	MIX_ENV=test mix recreate_db
	MIX_ENV=test mix rollback --all
	MIX_ENV=test mix recreate_db

recreate-db:
	mix recreate_db
	MIX_ENV=test mix recreate_db

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
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	cd assets && NODE_ENV=production yarn deploy
	MIX_ENV=prod mix phx.digest
	NODE_ENV=production MIX_ENV=prod mix distillery.release

release-local:
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	cd assets && NODE_ENV=production yarn deploy
	MIX_ENV=prod mix phx.digest
	NODE_ENV=production MIX_ENV=prod mix do compile, distillery.release --env=local --no-tar

lint: eslint flow
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

flow:
	cd assets && node_modules/.bin/flow check

eslint:
	cd assets && yarn lint

mocha:
	cd assets && NODE_PATH=test/js:js npm test

check-warnings:
	MIX_ENV=dev mix compile --warnings-as-errors --all-warnings
	MIX_ENV=test mix compile --warnings-as-errors --all-warnings
	MIX_ENV=prod mix compile --warnings-as-errors --all-warnings

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted

fix:
	mix format --check-equivalent
	cd assets && yarn fix

psql:
	psql -d air_dev -h localhost -p 20002 -U air -w 
	