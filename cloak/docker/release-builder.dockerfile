FROM aircloak/cloak_builder_base
MAINTAINER Aircloak

# Copy the remaining folders
COPY cloak/lib /aircloak/cloak/lib
COPY cloak/include /aircloak/cloak/include
COPY cloak/rel /aircloak/cloak/rel
COPY docker_cache/cloak/deps /aircloak/cloak/deps
COPY docker_cache/cloak/_build /aircloak/cloak/_build

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/cloak && \
  make release
