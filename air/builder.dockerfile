FROM aircloak/nodejs:$NODEJS_VERSION
MAINTAINER Aircloak

# Install dependencies and configure UTF-8
RUN \
  apt-get update && \
  apt-get install jq ruby-full -y && \
  gem install bundle

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY air/mix.exs air/mix.lock air/package.json air/yarn.lock /aircloak/air/
COPY air/config /aircloak/air/config
COPY common /aircloak/common
COPY air/fetch_deps.sh /aircloak/air/
COPY air/docs /aircloak/air/docs

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/air && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.asdf/asdf.sh && MIX_ENV=prod mix deps.compile " && \
  echo "Fetching node packages..." && \
  bash -c ". ~/.bashrc && yarn install" && \
  cd docs && bundle install -j4 && cd ..

# Build the Bill of Materials
COPY bom /aircloak/bom
COPY cloak /aircloak/cloak
RUN \
  cd /aircloak/cloak && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  cd /aircloak/bom && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.asdf/asdf.sh && mix deps.compile " && \
  bash -c ". ~/.asdf/asdf.sh && mkdir /aircloak/air/priv " && \
  bash -c ". ~/.asdf/asdf.sh && mix bom --elixir /aircloak/cloak/deps --elixir /aircloak/air/deps --node /aircloak/air/node_modules /aircloak/air/priv/bom.json "

# Now we copy the rest of the site and build the release.
COPY air /aircloak/air
RUN bash -c ". ~/.asdf/asdf.sh && cd /aircloak/air && make release"
