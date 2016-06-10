FROM elixir:1.2.3
MAINTAINER Aircloak

MPI_INIT

# Install dependencies (packages, hex, rebar, npm, brunch) and configure UTF-8
RUN \
  apt-get update && \
  apt-get install locales wget curl git build-essential libz-dev -y && \
  . /tmp/build_config/proxies.sh && \
  mkdir -p $HOME && \
  mix local.hex --force && \
  mix local.rebar --force && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8

ENV LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8

COPY cloak /aircloak/cloak
COPY common /aircloak/common

WORKDIR /aircloak/cloak
