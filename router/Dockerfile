FROM nginx:1.9.3
MAINTAINER Sebastian Probst Eide <sebastian@aircloak.com>

ENV site_path /aircloak/balancer

RUN mkdir -p $site_path

RUN mkdir -p /tmp/build_config && echo '' > /tmp/build_config/proxies.sh
COPY image_shell_init.sh /tmp/build_config/
RUN . /tmp/build_config/image_shell_init.sh

RUN rm /etc/nginx/conf.d/default.conf

RUN apt-get update && apt-get install -y vim nano curl && \
  curl -L \
    https://github.com/kelseyhightower/confd/releases/download/v0.10.0/confd-0.10.0-linux-amd64 \
    -o /usr/local/bin/confd && \
  chmod +x /usr/local/bin/confd

EXPOSE 8200

RUN mkdir -p /etc/confd/conf.d && mkdir -p /etc/confd/templates
COPY docker $site_path/docker

USER root