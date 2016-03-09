.PHONY: start test migrate rollback

start:
	iex -S mix phoenix.server

test: test-eunit
	mix test

test-eunit:
	MIX_ENV=test mix compile
	MIX_ENV=test mix eunit

test-coverage:
	MIX_ENV=test mix coveralls.html
	@printf "\e[33;32mReport is stored in cover/excoveralls.html\e[0m\n"

migrate:
	mix ecto.migrate

rollback:
	mix ecto.rollback

dialyze:
	MIX_ENV=dev scripts/dialyze.sh

docs:
	MIX_ENV=dev mix docs
