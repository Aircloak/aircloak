FROM aircloak/phoenix:$NODEJS_VERSION

# First we'll copy only the subset of needed files and compile deps
# This will reduce the amount of rebuilding when only the source code is changed.
COPY central/mix.exs central/mix.lock /aircloak/central/
COPY central/assets/package.json central/assets/yarn.lock /aircloak/central/assets/
COPY central/config /aircloak/central/config
COPY common /aircloak/common
COPY central/fetch_deps.sh /aircloak/central/

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/central && \
  bash -c ". ~/.asdf/asdf.sh && ./fetch_deps.sh --only prod" && \
  echo "Fetching node packages..." && \
  bash -c ". ~/.bashrc && cd assets && yarn install" && \
  bash -c ". ~/.asdf/asdf.sh && MIX_ENV=prod mix deps.compile"

# Now we copy the rest of the site and build the release.
COPY central /aircloak/central
RUN bash -c ". ~/.asdf/asdf.sh && cd /aircloak/central && make release"
