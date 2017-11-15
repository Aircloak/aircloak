SHELL=/bin/bash -o pipefail

.PHONY: sandbox deps docs proper proper-std proper-extended test clean all compile
.PHONY: recreate-db recreate-perf-db dialyze lint release release-local perftest dev-container
.PHONY: odbc_drivers odbc_drivers_folder
.PHONY: sql-server-container sql-server-database
.PHONY: mysql-server-container mysql-server-database
.PHONY: mongo-server-container
.PHONY: ci.compliance ci.compliance.debug

define start_container
  if [ "$$(docker ps -a | grep $(1))" ]; then docker start $(1); else docker run --name $(1) $(2); fi
endef

all: deps compile

compile: odbc_drivers
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
	TRAVIS=true mix test --include exclude_in_dev --include compliance

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
	mix release

release-local:
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix compile.app --force
	mix release --env=local --no-tar

perftest: deps
	MIX_ENV=prod mix compile
	PERFORMANCE_TEST=true MIX_ENV=prod mix run perftest/main.exs

dev-container: odbc_drivers
	./dev_container/build-image.sh
	./dev_container/container.sh console

sql-server-container:
	$(call start_container, aircloak_sql_server, -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' -p 1433:1433 -d microsoft/mssql-server-linux:2017-latest)

sql-server-database:
	docker exec -it aircloak_sql_server /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P 7fNBjlaeoRwz*zH9 -Q "CREATE DATABASE $(DB_NAME)"

mysql-server-container:
	$(call start_container, aircloak_mysql_server, -e MYSQL_ALLOW_EMPTY_PASSWORD=true -p 3306:3306 -d mysql:5.7.19)

mysql-server-database:
	docker exec -it aircloak_mysql_server mysql -u root -e "CREATE DATABASE $(DB_NAME)"

mongo-server-container:
	$(call start_container, aircloak_mongodb_server, -p 27017:27017 -d mongo)

odbc_drivers: odbc_drivers_folder priv/odbc/drivers/libodbc-sap-hana-v2.so

odbc_drivers_folder:
	mkdir -p priv/odbc/drivers

priv/odbc/drivers/libodbc-sap-hana-v2.so:
	wget -P priv/odbc/drivers https://www.dropbox.com/s/aerstjlatecny79/libodbc-sap-hana-v2.so

ci.compliance:
	../ci/run.sh cloak_compliance

ci.compliance.debug:
	../ci/run.sh debug_cloak_compliance
