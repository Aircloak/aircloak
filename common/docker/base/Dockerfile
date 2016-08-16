FROM debian:8.5
MAINTAINER Aircloak

MPI_INIT

# locale setup for properly working Erlang nodes
RUN \
  apt-get update && \
  apt-get install -y locales wget curl git nano telnet && \
  . /tmp/build_config/proxies.sh && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8
ENV LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8

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
