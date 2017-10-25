FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

RUN apt-get install -y unixodbc odbc-postgresql libmyodbc libaio1 inotify-tools
COPY cloak/priv/odbc/docker/odbc.ini /etc/
COPY cloak/priv/odbc/docker/sqlserver_setup.sh /aircloak/
RUN /aircloak/sqlserver_setup.sh

COPY cloak/dev_container/docker/start.sh /start.sh

CMD /start.sh
