FROM debian:$DEBIAN_VERSION

MPI_INIT

# Support for rebuilding of all images. Change this date if you want to
# force the rebuild of all images. Usually you want to do this if you
# want to upgrade Debian packages on all images.
RUN echo 20210616 > /dev/null

# locale setup for properly working Erlang nodes
# note: we need to kill dirmngr after installing gnupg, otherwise
#       it won't be able to contact the keyservers due to a bug.
RUN \
  apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y locales wget curl git nano telnet gnupg2 gosu && \
  . /tmp/build_config/proxies.sh && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8; \
  gosu nobody true

ENV LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8
