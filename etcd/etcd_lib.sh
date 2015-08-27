function log {
  msg=$1
  echo "[aircloak] $msg"
}

function silence_etcd_set {
  export SILENT_ETCD_SET="true"
}

function init_env {
  if [ -n "$(env | grep boot2docker)" ]; then
    log "Assuming using boot2docker due to environment variables"
    ETCD_DEFAULT_IP=$(boot2docker ip)
  else
    ETCD_DEFAULT_IP="127.0.0.1"
  fi

  export ETCD_PORT=${AIR_ETCD_PORT:-4002}
  export HOST_IP=${ETCD_HOST_IP:-$ETCD_DEFAULT_IP}
  export ETCD=$HOST_IP:$ETCD_PORT
}

function etcd_is_up {
  curl --silent http://$ETCD/version > /dev/null
  if [ $? -ne 0 ]; then
    return 1
  else
    return 0
  fi
}

function wait_for_etcd {
  # Crudely spin-lock, waiting for etcd to become available
  until etcd_is_up; do
    log "etcd not yet running..."
    sleep 0.1
  done
  log "Etcd is running"
}

function etcd_set {
  path=$1
  value=$2
  if [ -z "$SILENT_ETCD_SET" ]; then
    log "$ETCD: setting etcd: $path = $value"
  else
    log "Setting etcd: $path = XXXX"
  fi
  curl -XPUT -L --silent http://$ETCD/v2/keys$path -d value="$value" > /dev/null
}

function etcd_get {
    echo $(
      curl -s -L http://$ETCD/v2/keys$1 \
        | jq '.node.value' \
        | sed s/\"//g
    )
}

function docker_start_args {
  echo "-p ${ETCD_PORT}:${ETCD_PORT} $1 \
      quay.io/coreos/etcd:v2.0.6 \
      -name etcd0 \
      -advertise-client-urls http://${ETCD_DEFAULT_IP}:2379,http://${ETCD_DEFAULT_IP}:${ETCD_PORT} \
      -listen-client-urls http://0.0.0.0:2379,http://0.0.0.0:${ETCD_PORT} \
      -initial-advertise-peer-urls http://${ETCD_DEFAULT_IP}:2380 \
      -listen-peer-urls http://0.0.0.0:2380 \
      -initial-cluster-token etcd-cluster-1 \
      -initial-cluster etcd0=http://${ETCD_DEFAULT_IP}:2380 \
      -initial-cluster-state new"
}

function tcp_ports {
  environment_offset=$(
        cat "$(dirname ${BASH_SOURCE[0]})/tcp_ports.json" |
            egrep -v '//' |
            jq --raw-output '.environment_offsets.'$1
      )

  if [ "$environment_offset" = "null" ]; then
    echo "Invalid env provided: $1" >&2
    exit 1
  fi

  service_offsets=$(
        cat "$(dirname ${BASH_SOURCE[0]})/tcp_ports.json" |
            egrep -v '//' |
            jq --raw-output '.service_offsets | to_entries | map("\(.key) \(.value)")|.[]'
      )

  while read line; do
    IFS=" " read service_name service_offset < <(echo "$line")
    echo "$service_name $(($environment_offset + $service_offset))"
  done < <(echo "$service_offsets")
}

function set_tcp_ports {
  while read line; do
    IFS=" " read service port < <(echo "$line")
    etcd_set "/tcp_ports/$service" $port
  done < <(echo "$(tcp_ports $1)")
}

function get_tcp_port {
  tcp_ports $1 | egrep "^$2" | awk '{print $2}'
}
