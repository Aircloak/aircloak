FROM aircloak/base
MAINTAINER Aircloak

## ------------------------------------------------------------------
## Create user and copy in app
## ------------------------------------------------------------------

# User under which the app will run.
RUN useradd --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/cloak

COPY cloak/artifacts/rel /aircloak/cloak
COPY cloak/docker/start.sh /aircloak/

RUN chown -R deployer:deployer /aircloak/cloak && chown -R deployer:deployer /var/run/

# We'll run as root, but step down in the init script to the non-privileged user
USER root

CMD /aircloak/start.sh

VOLUME /runtime_config

TAG_VERSION
