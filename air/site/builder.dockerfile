FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

# Install dependencies (packages, npm, brunch) and configure UTF-8
RUN \
  apt-get update && \
  apt-get install jq ruby-full -y && \
  . /tmp/build_config/proxies.sh && \
  wget -qO- https://nodejs.org/download/release/v5.8.0/node-v5.8.0-linux-x64.tar.gz | tar xzv && \
  mv node-v5.8.0-linux-x64 /usr/local/node && \
  /usr/local/node/bin/npm install -g brunch && \
  gem install bundle

ENV PATH=/usr/local/node/bin:$PATH

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY air/config/config.sh air/config/tcp_ports.json /aircloak/air/config/
COPY air/etcd /aircloak/air/etcd
COPY air/site/mix.exs air/site/mix.lock air/site/package.json /aircloak/air/site/
COPY air/site/config /aircloak/air/site/config
COPY common /aircloak/common
COPY air/site/fetch_deps.sh /aircloak/air/site/
COPY air/site/docs /aircloak/air/site/docs

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/air/site && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.asdf/asdf.sh && MIX_ENV=prod mix deps.compile " && \
  echo "Fetching npm packages..." && \
  npm install && \
  cd docs && bundle install -j4 && cd ..

# Build the Bill of Materials
COPY bom /aircloak/bom
COPY cloak /aircloak/cloak
RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/cloak && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  cd /aircloak/bom && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.asdf/asdf.sh && mix deps.compile " && \
  bash -c ". ~/.asdf/asdf.sh && mkdir /aircloak/air/site/priv " && \
  bash -c ". ~/.asdf/asdf.sh && mix bom --elixir /aircloak/cloak/deps --elixir /aircloak/air/site/deps --node /aircloak/air/site/node_modules /aircloak/air/site/priv/bom.json "

# Now we copy the rest of the site and build the release.
COPY air/site /aircloak/air/site
RUN bash -c ". ~/.asdf/asdf.sh && cd /aircloak/air/site && make release"
