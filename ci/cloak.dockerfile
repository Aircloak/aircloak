FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

ENV CI=true
ENV DEPLOY_CONFIG=dockerized_ci

RUN apt-get install -y unixodbc odbc-postgresql libmyodbc libaio1 inotify-tools
COPY cloak/priv/odbc/docker/odbc.ini /etc/
COPY cloak/priv/odbc/docker/sqlserver_setup.sh /aircloak/
RUN /aircloak/sqlserver_setup.sh

WORKDIR /aircloak/cloak
