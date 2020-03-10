FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

# ---------------------------------------------------------------------
# Create user and copy in app
# ---------------------------------------------------------------------

RUN apt-get -y install postgresql-11

COPY air/docker/pg_hba.conf /etc/postgresql/11/main/

# User under which the app will run.
RUN useradd --create-home --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/air

RUN \
  chown -R deployer:deployer /aircloak/air/ && \
  chown -R deployer:deployer /var/run/ && \
  chown -R postgres:postgres /var/run/postgresql

COPY --chown=deployer:deployer air/artifacts/rel /aircloak/air
COPY air/docker/start.sh /aircloak/
RUN sed -i 's|$VERSION|$RELEASE_VERSION|g' /aircloak/start.sh

# We'll run as root, but step down in the init script to the non-privileged user
USER root

CMD /aircloak/start.sh

VOLUME /runtime_config

TAG_VERSION
