FROM debian:jessie
MAINTAINER Aircloak

AIR_INIT

## ------------------------------------------------------------------
## Get dependencies needed on the build system
## ------------------------------------------------------------------

RUN \
  apt-get update && \
  apt-get install \
    build-essential libssl-dev git curl libncurses5-dev libprotobuf-c-dev protobuf-c-compiler \
    liblua5.1-0-dev \
    -y

## ------------------------------------------------------------------
## Get erlang installed
## ------------------------------------------------------------------

RUN . /tmp/build_config/proxies.sh && \
  curl -L -O http://www.erlang.org/download/otp_src_17.5.tar.gz && \
  mkdir -p /tmp/download && \
  mv /otp_src_17.5.tar.gz /tmp/download/otp.tar.gz

WORKDIR /tmp/download
RUN tar -zxvf otp.tar.gz && rm otp.tar.gz

WORKDIR /tmp/download/otp_src_17.5
RUN \
  ./configure --disable-hipe && \
  make && \
  make install


## ------------------------------------------------------------------
## Configure user and get app in place
## ------------------------------------------------------------------

# User under which the app will run.
RUN /tmp/build_config/useradd.sh --create-home --shell /bin/bash deployer

RUN mkdir -p /aircloak/utils && chown deployer:deployer /aircloak/utils

USER root

# In order to clone from Github inside the docker image,
# we unfortunately need to relax our host checking...
RUN mkdir -p /home/deployer/.ssh && mkdir -p /tmp/web/backend

# First build dependencies. This ensures that a code change won't result in
# full rebuilding of dependencies.
COPY backend/artifacts/cache/deps /tmp/web/backend/deps
COPY backend/rebar backend/rebar.config backend/rebar.config.lock /tmp/web/backend/
RUN cd /tmp/web/backend && ./rebar compile

# Then copy required sources and build the release
COPY config/config.sh /tmp/web/config/config.sh
COPY config/tcp_ports.json /tmp/web/config/tcp_ports.json
COPY backend/apps /tmp/web/backend/apps
COPY backend/include /tmp/web/backend/include
COPY backend/rel /tmp/web/backend/rel
COPY backend/generate_cloak_conf.escript /tmp/web/backend/
COPY backend/Makefile backend/copy_configs.sh /tmp/web/backend/
RUN cd /tmp/web/backend/ && make rel

AIR_TAG_VERSION
