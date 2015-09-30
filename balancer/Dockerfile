FROM debian:jessie
MAINTAINER Aircloak

AIR_INIT

RUN apt-get update && apt-get install -y haproxy=1.5.8-3+deb8u2 rsyslog vim nano curl jq
COPY balancer/docker config/config.sh config/tcp_ports.json /aircloak/balancer/
COPY balancer/docker/49-haproxy.conf /etc/rsyslog.d/
USER root

AIR_TAG_VERSION
