FROM elixir:1.2.3
MAINTAINER Aircloak

MPI_INIT

# Install dependencies (packages, hex, rebar, npm, brunch) and configure UTF-8
RUN \
  apt-get update && \
  apt-get install locales wget curl git build-essential libz-dev -y && \
  . /tmp/build_config/proxies.sh && \
  mkdir -p $HOME && \
  mix local.hex --force && \
  mix local.rebar --force && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8

ENV LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY cloak/mix.exs cloak/mix.lock /aircloak/cloak/
COPY cloak/config/config.exs cloak/config/prod.exs /aircloak/cloak/config/
COPY common /aircloak/common
COPY cloak/fetch_deps.sh /aircloak/cloak/

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/cloak && \
  MIX_ENV=prod ./fetch_deps.sh --only prod && \
  MIX_ENV=prod mix deps.compile

# Now we copy the rest of the site and build the release.
COPY cloak /aircloak/cloak
RUN cd /aircloak/cloak && make release
