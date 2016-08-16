FROM debian:8.5
MAINTAINER Aircloak

MPI_INIT

RUN \
  apt-get update && \
  apt-get install -y locales && \
  . /tmp/build_config/proxies.sh && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8

ENV LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8
