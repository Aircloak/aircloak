.PHONY: start test migrate rollback

start:
	mix start

start2:
	mix start 2

start3:
	mix start 2

test: test-eunit
	mix test

test-eunit:
	MIX_ENV=test mix compile
	mix eunit

test.cover:
	mix test.cover

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
