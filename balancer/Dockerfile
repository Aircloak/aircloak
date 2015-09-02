FROM debian:jessie
MAINTAINER Aircloak

RUN mkdir -p /tmp/build_config && echo '' > /tmp/build_config/proxies.sh
COPY image_shell_init.sh /tmp/build_config/
RUN . /tmp/build_config/image_shell_init.sh

RUN apt-get update && apt-get install -y build-essential zlib1g-dev libpcre3 libpcre3-dev libbz2-dev libssl-dev vim nano curl wget

RUN . /tmp/build_config/proxies.sh && wget http://nginx.org/download/nginx-1.9.4.tar.gz && \
  tar -vxzf nginx-1.9.4.tar.gz && \
  cd nginx-1.9.4 && \
  ./configure --with-stream && \
  make && \
  make install

COPY docker /aircloak/balancer
USER root