FROM debian:jessie
MAINTAINER Aircloak

MPI_INIT

COPY .tool-versions /aircloak/
RUN \
  apt-get update && \
  apt-get install -y locales wget libz-dev curl autoconf libncurses-dev build-essential libssl-dev unzip git && \
  . /tmp/build_config/proxies.sh && \
  mkdir -p $HOME && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8 && \
  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.1.0 && \
  bash -c ". ~/.asdf/asdf.sh && asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git" && \
  bash -c ". ~/.asdf/asdf.sh && asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git" && \
  bash -c ". ~/.asdf/asdf.sh && cd /aircloak && asdf install" && \
  bash -c ". ~/.asdf/asdf.sh && cd /aircloak/ && mix local.hex --force && mix local.rebar --force"

ENV LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8
RUN echo '. ~/.asdf/asdf.sh' >> ~/.bashrc
