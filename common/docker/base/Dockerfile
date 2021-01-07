FROM debian:$DEBIAN_VERSION

MPI_INIT

# Support for rebuilding of all images. Change this date if you want to
# force the rebuild of all images. Usually you want to do this if you
# want to upgrade Debian packages on all images.
RUN echo 20210104 > /dev/null

# Setting alternative repositories because Jessie has been removed from mirrors
# See https://www.jesusamieiro.com/failed-to-fetch-http-ftp-debian-org-debian-dists-jessie-updates-main-404-not-found/
RUN printf '\\\n\
deb http://deb.debian.org/debian/ jessie main\\\n\
deb-src http://deb.debian.org/debian/ jessie main\\\n\
\\\n\
deb http://security.debian.org/ jessie/updates main\\\n\
deb-src http://security.debian.org/ jessie/updates main\\\n\
\\\n\
deb http://archive.debian.org/debian jessie-backports main\\\n\
deb-src http://archive.debian.org/debian jessie-backports main\\\n\
' > /etc/apt/sources.list && \
echo "Acquire::Check-Valid-Until false;" | tee -a /etc/apt/apt.conf.d/10-nocheckvalid

# locale setup for properly working Erlang nodes
RUN \
  apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y locales wget curl git nano telnet && \
  . /tmp/build_config/proxies.sh && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8
ENV LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8

# According to the official docs, it is recommended to use gosu instead of sudo
# to step-down from root.
# See https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#/user
# for details.
RUN \
  . /tmp/build_config/proxies.sh && \
  curl -o /usr/local/bin/gosu -sSL "https://github.com/tianon/gosu/releases/download/1.4/gosu-$(dpkg --print-architecture)" && \
  chmod +x /usr/local/bin/gosu
