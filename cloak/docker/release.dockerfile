FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

## ------------------------------------------------------------------
## Create user and copy in app
## ------------------------------------------------------------------

# User under which the app will run.
RUN useradd --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/cloak

COPY cloak/artifacts/rel /aircloak/cloak
COPY cloak/docker/start.sh /aircloak/
RUN sed -i 's|$VERSION|$RELEASE_VERSION|g' /aircloak/start.sh

RUN chown -R deployer:deployer /aircloak/cloak && chown -R deployer:deployer /var/run/

# We'll run as root, but step down in the init script to the non-privileged user
USER root

# Setup ODBC drivers
RUN apt-get install -y unixodbc odbc-postgresql libmyodbc
COPY cloak/priv/odbc/docker/odbc.ini /etc/
COPY cloak/priv/odbc/docker/sqlserver_setup.sh /aircloak/
COPY cloak/priv/odbc/docker/msodbcsql-13.0.0.0.tar.gz /aircloak/
RUN /aircloak/sqlserver_setup.sh

CMD /aircloak/start.sh

VOLUME /runtime_config

TAG_VERSION
