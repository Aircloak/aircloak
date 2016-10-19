FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

# Install dependencies (packages, npm, brunch) and configure UTF-8
RUN \
  apt-get update && \
  apt-get install jq ruby-full -y && \
  . /tmp/build_config/proxies.sh && \
  wget -qO- https://nodejs.org/download/release/v5.8.0/node-v5.8.0-linux-x64.tar.gz | tar xzv && \
  mv node-v5.8.0-linux-x64 /usr/local/node && \
  /usr/local/node/bin/npm install -g brunch

ENV PATH=/usr/local/node/bin:$PATH

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY central/mix.exs central/mix.lock central/package.json central/npm-shrinkwrap.json /aircloak/central/
COPY central/config /aircloak/central/config
COPY common /aircloak/common
COPY central/fetch_deps.sh /aircloak/central/

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/central && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.asdf/asdf.sh && MIX_ENV=prod mix deps.compile " && \
  echo "Fetching npm packages..." && \
  npm install

# Workaround for node vs docker's filesystem interaction. See https://github.com/npm/npm/issues/9863
RUN \
  cd /aircloak/central && \
  mv node_modules node_modules.tmp && mv node_modules.tmp node_modules && npm install && npm prune

# Now we copy the rest of the site and build the release.
COPY central /aircloak/central
RUN bash -c ". ~/.asdf/asdf.sh && cd /aircloak/central && make release"
