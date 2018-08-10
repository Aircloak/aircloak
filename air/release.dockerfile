FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

# ---------------------------------------------------------------------
# Create user and copy in app
# ---------------------------------------------------------------------

RUN \
  echo "deb http://apt.postgresql.org/pub/repos/apt/ jessie-pgdg main" >> /etc/apt/sources.list.d/pdgd.list && \
  wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
  apt-get update && \
  apt-get -y install postgresql-9.6

COPY air/docker/pg_hba.conf /etc/postgresql/9.6/main/

# User under which the app will run.
RUN useradd --create-home --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/air

COPY air/artifacts/rel /aircloak/air
COPY air/docker/start.sh /aircloak/
RUN sed -i 's|$VERSION|$RELEASE_VERSION|g' /aircloak/start.sh

RUN \
  chown -R deployer:deployer /aircloak/air && \
  chown -R deployer:deployer /var/run/ && \
  chown -R postgres:postgres /var/run/postgresql

# We'll run as root, but step down in the init script to the non-privileged user
USER root

CMD /aircloak/start.sh

VOLUME /runtime_config

TAG_VERSION
