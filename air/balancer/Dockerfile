FROM debian:jessie
MAINTAINER Aircloak

MPI_INIT

RUN apt-get update && \
    apt-get install -y haproxy=1.5.8-3+deb8u2 netcat vim nano curl jq && \
    apt-get -y upgrade
COPY air/balancer/docker air/config/config.sh air/config/tcp_ports.json /aircloak/balancer/
USER root

TAG_VERSION
