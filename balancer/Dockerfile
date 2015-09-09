FROM debian:jessie
MAINTAINER Aircloak

RUN air_init

RUN apt-get update && \
  apt-get install -y build-essential zlib1g-dev libpcre3 libpcre3-dev libbz2-dev libssl-dev \
  vim nano curl wget jq

RUN . /tmp/build_config/proxies.sh && wget http://nginx.org/download/nginx-1.9.4.tar.gz && \
  tar -vxzf nginx-1.9.4.tar.gz && \
  cd nginx-1.9.4 && \
  ./configure --with-stream && \
  make && \
  make install

COPY balancer/docker /aircloak/balancer
COPY config/* /aircloak/balancer/
USER root