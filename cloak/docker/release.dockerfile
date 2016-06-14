FROM debian:jessie
MAINTAINER Aircloak

## ------------------------------------------------------------------
## Some basic setup of the image
## ------------------------------------------------------------------

MPI_INIT

# Dependencies, helper tools, and configuration of UTF-8 locale
RUN \
  apt-get update && \
  apt-get install locales nano telnet curl -y && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8

ENV SHELL=/bin/bash TERM=xterm LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8

# According to many advices, we'll use gosu instead of sudo to step-down from root
# Apparently, there are some issues with sudo inside the docker container.
# For example, Docker docs (https://docs.docker.com/articles/dockerfile_best-practices/)
# state following:
# You should avoid installing or using sudo since it has unpredictable TTY and
# signal-forwarding behavior that can cause more problems than it solves. If you
# absolutely need functionality similar to sudo (e.g., initializing the daemon as
# root but running it as non-root), you may be able to use gosu.
RUN \
  . /tmp/build_config/proxies.sh && \
  curl -o /usr/local/bin/gosu -sSL "https://github.com/tianon/gosu/releases/download/1.4/gosu-$(dpkg --print-architecture)" && \
  chmod +x /usr/local/bin/gosu

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
