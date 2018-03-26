FROM aircloak/rust:$RUST_VERSION
MAINTAINER Aircloak

COPY common /aircloak/common
COPY cloak /aircloak/cloak
COPY cloak/lib /aircloak/cloak/lib
COPY cloak/include /aircloak/cloak/include
COPY cloak/rel /aircloak/cloak/rel
COPY VERSION /aircloak/
COPY $CLOAK_CACHE/deps /aircloak/cloak/deps
COPY $CLOAK_CACHE/_build /aircloak/cloak/_build
COPY $CLOAK_CACHE/.cargo /root/.cargo

RUN \
  . /tmp/build_config/proxies.sh && \
  cd /aircloak/cloak && \
  bash -c ". ~/.asdf/asdf.sh && make release"
