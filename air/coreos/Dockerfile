FROM busybox:latest
MAINTAINER Aircloak

COPY air/coreos/docker /aircloak/air
RUN mkdir -p /aircloak/air/static_website && \
    mv /aircloak/air/static_website_container.sh /aircloak/air/static_website/container.sh
COPY air/config/config.sh air/config/tcp_ports.json /aircloak/air/config/
COPY air/etcd /aircloak/air/etcd/
COPY docker/docker_helper.sh /aircloak/docker/
COPY air/site/container.sh /aircloak/air/site/
COPY air/router/container.sh /aircloak/air/router/

AIR_TAG_VERSION
