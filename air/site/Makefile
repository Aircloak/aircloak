.PHONY: start test migrate rollback

start:
	iex -S mix phoenix.server

test: test-eunit
	mix test

test-eunit:
	MIX_ENV=test mix compile
	MIX_ENV=test mix eunit

migrate:
	mix ecto.migrate

rollback:
	mix ecto.rollback
