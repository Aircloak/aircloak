.PHONY: start test migrate rollback

start:
	iex -S mix start

start2:
	iex -S mix start 2

start3:
	iex -S mix start 3

test:
	mix test.full

test.cover:
	mix test.cover

recreate_db:
	mix recreate_db

migrate:
	mix migrate

rollback:
	mix rollback

dialyze:
	mix dialyze

docs:
	mix docs

release:
	MIX_ENV=prod mix do compile, site_release

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint
