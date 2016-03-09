.PHONY: start test migrate rollback

start:
	iex -S mix phoenix.server

test: test-eunit
	mix test

test-eunit:
	MIX_ENV=test mix compile
	mix eunit

test-coverage:
	mix coveralls.html
	@printf "\e[33;32mReport is stored in cover/excoveralls.html\e[0m\n"

migrate:
	mix ecto.migrate

rollback:
	mix ecto.rollback

dialyze:
	scripts/dialyze.sh

docs:
	mix docs

release:
	MIX_ENV=prod mix do deps.get, compile
	brunch build --production
	mix phoenix.digest
	mix release
