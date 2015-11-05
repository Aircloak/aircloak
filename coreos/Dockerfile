FROM busybox:latest
MAINTAINER Aircloak

COPY coreos/docker /aircloak/air
COPY config/config.sh config/tcp_ports.json /aircloak/air/config/
COPY etcd /aircloak/air/etcd/
COPY common/docker_helper.sh /aircloak/air/common/
COPY frontend/container.sh /aircloak/air/frontend/
COPY backend/container.sh /aircloak/air/backend/
COPY router/container.sh /aircloak/air/router/

AIR_TAG_VERSION