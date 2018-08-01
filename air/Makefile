.PHONY: all compile deps clean start start2 start3 test test-migrations recreate-db migrate rollback dialyze docs
.PHONY: release release-local lint mocha format check-format

all: deps compile

compile:
	mix compile
	mix compile.app --force

deps:
	mix deps.get
	cd assets && yarn install && cd ..
	cd docs && yarn install && cd ..

clean:
	mix clean --deps

start: compile
	iex -S mix phx.server

test: mocha test-migrations
	mix test.standard

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
	NODE_ENV=production MIX_ENV=prod mix site_release

release-local:
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	NODE_ENV=production MIX_ENV=prod mix do compile, site_release --env=local --no-tar

lint: eslint flow
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

flow:
	cd assets && node_modules/.bin/flow check

eslint:
	cd assets && node_modules/.bin/eslint --max-warnings 0 --rulesdir eslint_rules js
	cd assets && node_modules/.bin/eslint --max-warnings 0 --rulesdir eslint_rules --config .eslintrc-test.js test

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
