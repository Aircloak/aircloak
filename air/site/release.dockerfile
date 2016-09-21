FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

## ------------------------------------------------------------------
## Create user and copy in app
## ------------------------------------------------------------------

# User under which the app will run.
RUN useradd --create-home --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/air

COPY air/site/artifacts/rel /aircloak/air
COPY air/site/docker/start.sh /aircloak/

RUN chown -R deployer:deployer /aircloak/air && chown -R deployer:deployer /var/run/

# We'll run as root, but step down in the init script to the non-privileged user
USER root

CMD /aircloak/start.sh

VOLUME /runtime_config

TAG_VERSION
