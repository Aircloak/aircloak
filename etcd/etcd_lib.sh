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

function etcd_set {
  path=$1
  value=$2
  if [ -z "$SILENT_ETCD_SET" ]; then
    log "Setting etcd: $path = $value"
  else
    log "Setting etcd: $path = XXXX"
  fi
  curl -XPUT -L --silent http://$ETCD/v2/keys$path -d value="$value" > /dev/null
}
