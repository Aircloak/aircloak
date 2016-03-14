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

test-coverage:
	mix coveralls.html
	@printf "\e[33;32mReport is stored in cover/excoveralls.html\e[0m\n"

migrate:
	mix migrate

rollback:
	mix rollback

dialyze:
	mix dialyze

docs:
	mix docs

release:
	rm -rf rel/air
	MIX_ENV=prod mix compile
	brunch build --production
	mix phoenix.digest
	mix release
