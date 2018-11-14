FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

# ---------------------------------------------------------------------
# Create user and copy in app
# ---------------------------------------------------------------------

# Setup ODBC drivers
RUN apt-get install -y unixodbc odbc-postgresql libmyodbc libaio1 inotify-tools
COPY cloak/priv/odbc/docker/odbc.ini /etc/
COPY cloak/priv/odbc/docker/sqlserver_setup.sh /aircloak/
RUN /aircloak/sqlserver_setup.sh

# User under which the app will run.
RUN useradd --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/cloak

RUN chown -R deployer:deployer /aircloak/cloak && chown -R deployer:deployer /var/run/

COPY --chown=deployer:deployer cloak/artifacts/rel /aircloak/cloak
COPY cloak/docker/start.sh /aircloak/
RUN sed -i 's|$VERSION|$RELEASE_VERSION|g' /aircloak/start.sh

# We'll run as root, but step down in the init script to the non-privileged user
USER root

# required by SAP IQ ODBC client
ENV LD_LIBRARY_PATH="/aircloak/cloak/priv/odbc/drivers/sapiq/lib64"
ENV ODBCINI="/etc/odbc.ini"

CMD /aircloak/start.sh

VOLUME /runtime_config
VOLUME /odbc_drivers
VOLUME /persist

TAG_VERSION
