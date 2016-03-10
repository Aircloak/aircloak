FROM elixir:1.2.3
MAINTAINER Aircloak

AIR_INIT

# Install dependencies (packages, hex, rebar, npm, brunch) and configure UTF-8
RUN \
  . /tmp/build_config/proxies.sh && \
  apt-get update && \
  apt-get install locales wget curl git build-essential jq -y && \
  mkdir -p $HOME && \
  mix local.hex --force && \
  mix local.rebar --force && \
  wget -qO- https://nodejs.org/download/release/v5.8.0/node-v5.8.0-linux-x64.tar.gz | tar xzv && \
  mv node-v5.8.0-linux-x64 /usr/local/node && \
  /usr/local/node/bin/npm install -g brunch && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8

ENV PATH=/usr/local/node/bin:$PATH LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY config/config.sh config/tcp_ports.json /aircloak/air/config/
COPY etcd /aircloak/air/etcd
COPY site/mix.exs site/mix.lock site/package.json /aircloak/air/site/
COPY site/artifacts/cache/deps /aircloak/air/site/deps
COPY site/config /aircloak/air/site/config

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/air/site && \
  mix hex.registry fetch && \
  MIX_ENV=prod mix deps.compile && \
  npm install

# Now we copy the rest of the site and build the release.
COPY site /aircloak/air/site
RUN cd /aircloak/air/site && make release
