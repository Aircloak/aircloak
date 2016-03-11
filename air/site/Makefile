.PHONY: start test migrate rollback

start:
	iex --sname insights1@localhost -S mix phoenix.server

start2:
	INSIGHTS_PORT_OFFSET=1 iex --sname insights2@localhost -S mix phoenix.server

start3:
	INSIGHTS_PORT_OFFSET=2 iex --sname insights3@localhost -S mix phoenix.server

test: test-eunit
	mix test

test-eunit:
	MIX_ENV=test mix compile
	mix eunit

test-coverage:
	mix coveralls.html
	@printf "\e[33;32mReport is stored in cover/excoveralls.html\e[0m\n"

migrate:
	mix do app.start, ecto.migrate

rollback:
	mix do app.start, ecto.rollback

dialyze:
	scripts/dialyze.sh

docs:
	mix docs

release:
	rm -rf rel/air
	MIX_ENV=prod mix compile
	brunch build --production
	mix phoenix.digest
	mix release
