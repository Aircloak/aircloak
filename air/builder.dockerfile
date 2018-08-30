FROM aircloak/phoenix:$NODEJS_VERSION
MAINTAINER Aircloak

# Install dependencies:
# - calibre: needed to compile offline versions of docs
RUN apt-get install calibre -y

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY air/mix.exs air/mix.lock /aircloak/air/
COPY air/assets/package.json air/assets/yarn.lock /aircloak/air/assets/
COPY air/config /aircloak/air/config
COPY common /aircloak/common
COPY air/fetch_deps.sh /aircloak/air/
COPY air/docs /aircloak/air/docs
COPY VERSION /aircloak/
COPY $AIR_CACHE/deps /aircloak/air/deps

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/air && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.bashrc && cd assets && yarn install" && \
  bash -c ". ~/.bashrc && cd docs && yarn install"

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
  bash -c ". ~/.asdf/asdf.sh && mix bom --elixir /aircloak/cloak/deps --rust /aircloak/cloak/src/rodbc --elixir /aircloak/air/deps --node /aircloak/air/assets/node_modules /aircloak/air/priv"

# Now we copy the rest of the site and build the release.
COPY $AIR_CACHE/_build /aircloak/air/_build
COPY air /aircloak/air
RUN bash -c ". ~/.asdf/asdf.sh && cd /aircloak/air && make release"
