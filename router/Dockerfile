FROM nginx:1.9.11
MAINTAINER Aircloak

AIR_INIT

ENV site_path /aircloak/router
RUN mkdir -p $site_path
RUN rm /etc/nginx/conf.d/default.conf

RUN apt-get update && apt-get install -y vim nano curl jq && \
    . /tmp/build_config/proxies.sh && \
    curl -L \
      https://github.com/kelseyhightower/confd/releases/download/v0.10.0/confd-0.10.0-linux-amd64 \
      -o /usr/local/bin/confd && \
    chmod +x /usr/local/bin/confd && \
    apt-get -y upgrade

RUN mkdir -p /etc/confd/conf.d && mkdir -p /etc/confd/templates
COPY router/docker $site_path/docker
COPY config/config.sh $site_path/docker/config.sh
COPY config/tcp_ports.json $site_path/docker/tcp_ports.json

USER root

AIR_TAG_VERSION
