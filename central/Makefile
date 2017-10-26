.PHONY: all compile deps clean start start2 start3 test recreate-db migrate rollback dialyze docs release release-local lint mocha

all: deps compile

compile:
	mix compile

deps:
	mix deps.get
	yarn install

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

lint: eslint flow
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

flow:
	node_modules/.bin/flow check

eslint:
	node_modules/.bin/eslint --max-warnings 0 --rulesdir eslint_rules web/static/js
	node_modules/.bin/eslint --max-warnings 0 --rulesdir eslint_rules --config .eslintrc-test.js test/js

mocha:
	NODE_PATH=test/js:web/static/js npm test
