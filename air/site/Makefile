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
	iex -S mix start

start2:
	iex -S mix start 2

start3:
	iex -S mix start 3

test:
	mix test.standard

recreate_db:
	mix recreate_db

migrate:
	mix migrate

rollback:
	mix rollback

dialyze:
	mix dialyze_retry

docs:
	mix docs.all

release:
	NODE_ENV=production MIX_ENV=prod mix do compile, site_release

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint
