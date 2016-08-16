FROM aircloak/base:$DEBIAN_VERSION
MAINTAINER Aircloak

## ------------------------------------------------------------------
## Create user and copy in app
## ------------------------------------------------------------------

# User under which the app will run.
RUN useradd --create-home --shell /bin/bash deployer && mkdir -p /aircloak/app

WORKDIR /aircloak/insights

COPY air/site/artifacts/rel /aircloak/insights
COPY air/site/docker/start.sh /aircloak/

RUN chown -R deployer:deployer /aircloak/insights && chown -R deployer:deployer /var/run/

# We'll run as root, but step down in the init script to the non-privileged user
USER root

CMD /aircloak/start.sh

TAG_VERSION
