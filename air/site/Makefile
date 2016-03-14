.PHONY: start test migrate rollback

start:
	mix start

start2:
	mix start 2

start3:
	mix start 2

test:
	# For some reason, we need to compile before starting the test. Otherwise, eunit test won't pick up the
	# latest  compiled binaries. This needs to be investigated further, but for now, we're just manually
	# invoking the compile task as a hacky workaround
	MIX_ENV=test mix compile
	mix test.full

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
