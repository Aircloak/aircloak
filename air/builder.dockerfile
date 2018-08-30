FROM aircloak/phoenix:$NODEJS_VERSION
MAINTAINER Aircloak

# Install dependencies:
# - calibre: needed to compile offline versions of docs
# - jq: needed to analyze package.json
RUN apt-get install jq calibre -y

# compile bom
COPY VERSION /aircloak/
COPY common /aircloak/common
COPY bom /aircloak/bom
RUN \
  cd /aircloak/bom && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.asdf/asdf.sh && mix compile"

# install gitbook
COPY air/docs/package.json air/docs/yarn.lock /aircloak/air/docs/
RUN . /tmp/build_config/proxies.sh && bash -c ". ~/.bashrc && cd /aircloak/air/docs && yarn install"
# Note: the following line has to be executed as a separate command, after `yarn install` has finished. Otherwise
# the /aircloak/air/docs/node_modules/gitbook/package.json will not be present, and interpolation will silently fail.
RUN . /tmp/build_config/proxies.sh && bash -c ". ~/.bashrc && cd /aircloak/air/docs && yarn run gitbook fetch $(cat /aircloak/air/docs/node_modules/gitbook/package.json | jq -r '.version')"

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY air/mix.exs air/mix.lock /aircloak/air/
COPY air/assets/package.json air/assets/yarn.lock /aircloak/air/assets/
COPY air/config /aircloak/air/config
COPY air/fetch_deps.sh /aircloak/air/
COPY $AIR_CACHE/deps /aircloak/air/deps

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/air && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  bash -c ". ~/.bashrc && cd assets && yarn install"

# Build the Bill of Materials
COPY cloak /aircloak/cloak
RUN \
  cd /aircloak/cloak && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod " && \
  cd /aircloak/bom && \
  bash -c ". ~/.asdf/asdf.sh && mkdir /aircloak/air/priv " && \
  bash -c ". ~/.asdf/asdf.sh && mix bom --elixir /aircloak/cloak/deps --rust /aircloak/cloak/src/rodbc --elixir /aircloak/air/deps --node /aircloak/air/assets/node_modules /aircloak/air/priv"

# Now we copy the rest of the site and build the release.
COPY $AIR_CACHE/_build /aircloak/air/_build
COPY air /aircloak/air
RUN bash -c ". ~/.asdf/asdf.sh && cd /aircloak/air && make release"
