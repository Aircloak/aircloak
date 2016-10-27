FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

## ------------------------------------------------------------------
## Create user and copy in app
## ------------------------------------------------------------------

# User under which the app will run.
RUN useradd --create-home --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/central

COPY central/artifacts/rel /aircloak/central
COPY central/docker/start.sh /aircloak/

RUN chown -R deployer:deployer /aircloak/central && chown -R deployer:deployer /var/run/

# We'll run as root, but step down in the init script to the non-privileged user
USER root

CMD /aircloak/start.sh

VOLUME /runtime_config

TAG_VERSION
