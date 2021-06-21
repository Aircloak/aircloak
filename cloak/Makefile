SHELL=/bin/bash -o pipefail

.PHONY: deps docs proper proper-std proper-extended test clean all compile
.PHONY: recreate-db recreate-perf-db dialyze lint release release-local perftest
.PHONY: odbc_drivers odbc_drivers_folder oracle_drivers oracle_drivers_folder cloudera-impala_drivers
.PHONY: sql-server-container sql-server-database
.PHONY: ci.compliance dev-container
.PHONY: format check-format

define start_container
  if [ "$$(docker ps -a | grep $(1))" ]; then docker start $(1); else docker run --name $(1) $(2); fi
endef

all: deps compile

compile: odbc_drivers
	mix compile
	mix compile.app --force

deps:
	mix deps.get

clean:
	mix clean --deps
	rm -rf priv/odbc/drivers

start: compile
	iex -S mix

test:
	mix test

test_all:
	mix test --include exclude_in_dev --include compliance

recreate-db:
	@echo "Assumption: you have a database user called postgres that is a superuser. If not, run: CREATE USER postgres WITH SUPERUSER"
	./datagen/recreate_db.sh

recreate-perf-db:
	./datagen/recreate_perf_db.sh

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

release: odbc_drivers_folder
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	MIX_ENV=prod mix distillery.release

release-local:
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	MIX_ENV=prod mix distillery.release --env=local --no-tar

perftest: deps
	MIX_ENV=prod mix compile
	PERFORMANCE_TEST=true MIX_ENV=prod mix run perftest/main.exs

odbc_drivers: odbc_drivers_folder oracle_drivers cloudera-impala_drivers

odbc_drivers_folder:
	mkdir -p priv/odbc/drivers

oracle_drivers: oracle_drivers_folder priv/odbc/drivers/oracle/instantclient_18_3

oracle_drivers_folder:
	mkdir -p priv/odbc/drivers/oracle

priv/odbc/drivers/oracle/instantclient_18_3:
	wget -O priv/odbc/drivers/oracle/instantclient-odbc-linux.zip https://www.dropbox.com/s/uq5btss0ani4rpd/instantclient-odbc-linux.x64-18.3.0.0.0dbru.zip?dl=0
	wget -O priv/odbc/drivers/oracle/instantclient-basic-linux.zip https://www.dropbox.com/s/fntfxihjd9p1ojk/instantclient-basic-linux.x64-18.3.0.0.0dbru.zip?dl=0
	unzip priv/odbc/drivers/oracle/instantclient-basic-linux.zip -d priv/odbc/drivers/oracle
	unzip priv/odbc/drivers/oracle/instantclient-odbc-linux.zip -d priv/odbc/drivers/oracle
	rm priv/odbc/drivers/oracle/instantclient-basic-linux.zip
	rm priv/odbc/drivers/oracle/instantclient-odbc-linux.zip

cloudera-impala_drivers: priv/odbc/drivers/cloudera/impalaodbc

priv/odbc/drivers/cloudera/impalaodbc:
	mkdir -p priv/odbc/drivers/cloudera/impalaodbc
	wget -O priv/odbc/drivers/cloudera/clouderaimpalaodbc_2.6.5.1005-2_amd64.zip https://www.dropbox.com/s/sqxdzovckj1avwz/clouderaimpalaodbc_2.6.5.1005-2_amd64.zip?dl=0
	unzip priv/odbc/drivers/cloudera/clouderaimpalaodbc_2.6.5.1005-2_amd64.zip -d priv/odbc/drivers/cloudera/impalaodbc
	rm priv/odbc/drivers/cloudera/clouderaimpalaodbc_2.6.5.1005-2_amd64.zip

ci.compliance:
	ci/compliance.sh compliance

dev-container:
	ci/compliance.sh dev

format:
	mix format --check-equivalent

check-format:
	mix format --check-formatted
