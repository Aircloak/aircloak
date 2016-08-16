FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

COPY common /aircloak/common
COPY cloak /aircloak/cloak
COPY cloak/lib /aircloak/cloak/lib
COPY cloak/include /aircloak/cloak/include
COPY cloak/rel /aircloak/cloak/rel
COPY docker_cache/cloak/deps /aircloak/cloak/deps
COPY docker_cache/cloak/_build /aircloak/cloak/_build

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/cloak && \
  bash -c ". ~/.asdf/asdf.sh && make release"
