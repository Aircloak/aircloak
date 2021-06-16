FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

# ---------------------------------------------------------------------
# Create user and copy in app
# ---------------------------------------------------------------------

WORKDIR /aircloak/cloak

# Setup ODBC drivers
RUN apt-get update
RUN apt-get install -y unixodbc odbc-postgresql libaio1 inotify-tools unzip
COPY cloak/priv/odbc/docker/odbc.ini /etc/
COPY cloak/priv/odbc/docker/sqlserver_setup.sh /aircloak/
RUN /aircloak/sqlserver_setup.sh

# Setup Oracle Instant Client
RUN \
  mkdir -p priv/odbc/drivers/oracle/ && \
  wget -O priv/odbc/drivers/oracle/instantclient-odbc-linux.zip https://www.dropbox.com/s/uq5btss0ani4rpd/instantclient-odbc-linux.x64-18.3.0.0.0dbru.zip?dl=0 && \
  wget -O priv/odbc/drivers/oracle/instantclient-basic-linux.zip https://www.dropbox.com/s/fntfxihjd9p1ojk/instantclient-basic-linux.x64-18.3.0.0.0dbru.zip?dl=0 && \
  unzip priv/odbc/drivers/oracle/instantclient-basic-linux.zip -d priv/odbc/drivers/oracle && \
  unzip priv/odbc/drivers/oracle/instantclient-odbc-linux.zip -d priv/odbc/drivers/oracle && \
  rm priv/odbc/drivers/oracle/instantclient-basic-linux.zip && \
  rm priv/odbc/drivers/oracle/instantclient-odbc-linux.zip
ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/aircloak/cloak/priv/odbc/drivers/oracle/instantclient_18_3
# Oracle locale
ENV NLS_LANG=.AL32UTF8

# User under which the app will run.
RUN useradd --shell /bin/bash deployer && mkdir -p /aircloak/app
RUN chown -R deployer:deployer /aircloak/cloak && chown -R deployer:deployer /var/run/

COPY --chown=deployer:deployer cloak/artifacts/rel /aircloak/cloak
COPY cloak/docker/start.sh /aircloak/
RUN sed -i 's|$VERSION|$RELEASE_VERSION|g' /aircloak/start.sh

# We'll run as root, but step down in the init script to the non-privileged user
USER root

CMD /aircloak/start.sh

VOLUME /runtime_config
VOLUME /odbc_drivers
VOLUME /persist

TAG_VERSION