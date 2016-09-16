.PHONY: all compile deps clean start start2 start3 test recreate_db migrate rollback dialyze docs release lint

all: deps compile

compile:
	mix compile

deps:
	mix deps.get
	npm install

clean:
	mix clean --deps

start:
	iex -S mix phoenix.server

test:
	mix test.standard

recreate_db:
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
	mix docs.all

release:
	NODE_ENV=production MIX_ENV=prod mix do compile, site_release

lint: eslint
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

eslint:
	node_modules/.bin/eslint --max-warnings 0 --rulesdir eslint_rules web/static/js/*.js web/static/js/**/*.js

check_dependent_apps:
	mix check_dependent_apps
