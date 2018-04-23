FROM debian:8.10
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

# According to the official docs, it is recommended to use gosu instead of sudo
# to step-down from root.
# See https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#/user
# for details.
RUN \
  . /tmp/build_config/proxies.sh && \
  curl -o /usr/local/bin/gosu -sSL "https://github.com/tianon/gosu/releases/download/1.4/gosu-$(dpkg --print-architecture)" && \
  chmod +x /usr/local/bin/gosu
