. $(dirname ${BASH_SOURCE[0]})/../config/config.sh

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function silence_etcd_set {
  export SILENT_ETCD_SET="true"
}

function init_env {
  export ETCD_CLIENT_PORT=$(get_tcp_port $1 etcd/client)
  export ETCD_PEER_PORT=$(get_tcp_port $1 etcd/peer)
}

function etcd_is_up {
  curl --silent http://127.0.0.1:$ETCD_CLIENT_PORT/version > /dev/null
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
    log "127.0.0.1:$ETCD_CLIENT_PORT: setting etcd: $path = $value"
  else
    log "Setting etcd: $path = XXXX"
  fi
  curl -XPUT -L --silent http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys$path -d value="$value" > /dev/null
}

function etcd_get {
    echo $(
      curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys$1 \
        | jq '.node.value' \
        | sed s/\"//g
    )
}

function etcd_container_args {
  echo "
      -name etcd0
      -advertise-client-urls http://127.0.0.1:$ETCD_CLIENT_PORT
      -listen-client-urls http://0.0.0.0:$ETCD_CLIENT_PORT
      -initial-advertise-peer-urls http://127.0.0.1:$ETCD_PEER_PORT
      -listen-peer-urls http://0.0.0.0:$ETCD_PEER_PORT
      -initial-cluster-token etcd-cluster-1
      -initial-cluster etcd0=http://127.0.0.1:$ETCD_PEER_PORT
      -initial-cluster-state new"
}


function set_tcp_ports {
  while read line; do
    IFS=" " read service port < <(echo "$line")
    etcd_set "/tcp_ports/$service" $port
  done < <(echo "$(tcp_ports $1)")
}
