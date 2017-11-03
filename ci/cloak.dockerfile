FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

RUN apt-get install -y unixodbc odbc-postgresql libmyodbc libaio1 inotify-tools postgresql-client

COPY cloak/priv/odbc/docker/odbc.ini /etc/
COPY cloak/priv/odbc/docker/sqlserver_setup.sh /aircloak/
RUN /aircloak/sqlserver_setup.sh


# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY VERSION RELEASE_EXPIRY_DATE /aircloak/
COPY common /aircloak/common
COPY cloak/mix.exs cloak/mix.lock cloak/fetch_deps.sh cloak/config /aircloak/cloak/

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/cloak && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh && mix deps.compile && MIX_ENV=test mix deps.compile"

COPY cloak /aircloak/cloak
RUN cp /aircloak/cloak/priv/odbc/docker/odbc.ini /etc/

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/cloak && \
  bash -c ". ~/.asdf/asdf.sh && mix config_sap_hana_test_schema && mix compile && MIX_ENV=test mix compile"

WORKDIR /aircloak/cloak
