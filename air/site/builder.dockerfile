FROM elixir:1.2.3
MAINTAINER Aircloak

AIR_INIT

# Install dependencies (packages, hex, rebar, npm, brunch) and configure UTF-8
RUN \
  apt-get update && \
  apt-get install locales wget curl git build-essential jq libz-dev ruby-full -y && \
  . /tmp/build_config/proxies.sh && \
  mkdir -p $HOME && \
  mix local.hex --force && \
  mix local.rebar --force && \
  wget -qO- https://nodejs.org/download/release/v5.8.0/node-v5.8.0-linux-x64.tar.gz | tar xzv && \
  mv node-v5.8.0-linux-x64 /usr/local/node && \
  /usr/local/node/bin/npm install -g brunch && \
  gem install bundle && \
  locale-gen en_US.UTF-8 en_us && \
  dpkg-reconfigure locales && \
  locale-gen C.UTF-8 && \
  /usr/sbin/update-locale LANG=C.UTF-8

ENV PATH=/usr/local/node/bin:$PATH LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY air/config/config.sh air/config/tcp_ports.json /aircloak/air/config/
COPY air/etcd /aircloak/air/etcd
COPY air/site/mix.exs air/site/mix.lock air/site/package.json /aircloak/air/site/
COPY air/site/config /aircloak/air/site/config
COPY common /aircloak/common
COPY air/site/fetch_deps.sh /aircloak/air/site/
COPY air/site/api_docs /aircloak/air/site/api_docs

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/air/site && \
  ./fetch_deps.sh --only prod && \
  MIX_ENV=prod mix deps.compile && \
  echo "Fetching npm packages..." && \
  npm install && \
  cd api_docs && bundle install -j4 && cd ..

# Now we copy the rest of the site and build the release.
COPY air/site /aircloak/air/site
RUN cd /aircloak/air/site && make release
