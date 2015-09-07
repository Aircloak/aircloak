FROM debian:jessie
MAINTAINER Aircloak

RUN mkdir -p /tmp/build_config && echo '' > /tmp/build_config/proxies.sh
COPY image_shell_init.sh /tmp/build_config/
RUN . /tmp/build_config/image_shell_init.sh


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
RUN mkdir -p /home/deployer/.ssh && mkdir -p /tmp/build

# First build dependencies. This ensures that a code change won't result in
# full rebuilding of dependencies.
COPY artifacts/cache/deps /tmp/build/deps
COPY rebar rebar.config rebar.config.lock /tmp/build/
RUN cd /tmp/build && ./rebar compile

# Then copy required sources and build the release
COPY apps /tmp/build/apps
COPY include /tmp/build/include
COPY rel /tmp/build/rel
COPY generate_cloak_conf.escript /tmp/build/
COPY Makefile /tmp/build/
RUN cd /tmp/build/ && make rel
