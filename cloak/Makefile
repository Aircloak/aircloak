SHELL=/bin/bash -o pipefail

.PHONY: sandbox deps docs proper proper-std proper-extended test clean all compile
.PHONY: recreate-db recreate-perf-db dialyze lint release release-local perftest dev-container
.PHONY: odbc_drivers odbc_drivers_folder

all: deps compile

compile:
	mix compile
	mix compile.app --force

sandbox:
	mix compile.sandbox

deps:
	mix deps.get

clean:
	mix clean --deps
	rm -rf odbc_drivers

start: compile
	iex -S mix

test:
	mix test

test_all:
	TRAVIS=true mix test --include exclude_in_dev

recreate-db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./datagen/recreate_db.sh
	USER_COUNT=100 DB_NAME=datasource_compliance ./datagen/generate_db.sh

recreate-perf-db:
	USER_COUNT=10000 DB_NAME=performance ./datagen/generate_db.sh

recreate-db-more-data: recreate-db
	./datagen/add_more_test_data.sh

docs:
	mix docs

# a proper run may take some time...
# Note: proper-full is temporarily removed because of big changes in the code.
# Once full tests are fixed, need to include it again.
proper: proper-std proper-extended

proper-std:
	MIX_ENV=test mix proper --level simple

proper-extended:
	MIX_ENV=test mix proper --level extended

dialyze:
	mix dialyze_retry

lint:
	MIX_ENV=dev mix lint
	MIX_ENV=test mix lint

release:
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	mix release

release-local:
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	mix release --env=local --no-tar

perftest: deps
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix run perftest/main.exs

dev-container: odbc_drivers
	./dev-container.sh

odbc_drivers: odbc_drivers_folder deps/odbc_drivers/libodbc-sap-hana-v2.so

odbc_drivers_folder:
	mkdir -p deps/odbc_drivers

deps/odbc_drivers/libodbc-sap-hana-v2.so:
	wget -P deps/odbc_drivers/ https://www.dropbox.com/s/aerstjlatecny79/libodbc-sap-hana-v2.so
