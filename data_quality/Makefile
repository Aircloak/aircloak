.PHONY: all compile deps clean test dialyze docs lint format check-format

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

data_quality:
	mix data_quality

recreate-db:
	psql -c "DROP DATABASE IF EXISTS data_quality"
	psql -c "DROP ROLE IF EXISTS data_quality"
	psql -c "CREATE ROLE data_quality WITH LOGIN SUPERUSER"
	psql -c "CREATE DATABASE data_quality WITH OWNER data_quality"
	mix data_quality.generate_data --config config.json

setup-r:
	Rscript setup.r

clean:
	mix clean --deps

test:
	mix test

dialyze:
	mix dialyze_retry

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted
