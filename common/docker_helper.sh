#!/usr/bin/env bash

. $(dirname ${BASH_SOURCE[0]})/../config/config.sh

function push_to_registry {
  if named_container_running air_docker_registry; then
    REGISTRY_URL="127.0.0.1:$(get_tcp_port prod registry/http)"
    echo "Pushing $1 to local registry"
    docker tag -f aircloak/$1:latest $REGISTRY_URL/aircloak/$1:latest
    docker push $REGISTRY_URL/aircloak/$1:latest
  else
    echo "Warning: local registry is not running, image not pushed."
  fi
}

function named_container_running {
  if [ -z "$(docker ps --filter=name=$1 | grep -v CONTAINER)" ]; then
    return 1
  else
    return 0
  fi
}

function gracefully_stop_container {
  STOP_SIGNAL=${STOP_SIGNAL:-SIGTERM}
  echo "Sending $STOP_SIGNAL to container $1 for graceful shutdown"
  docker kill -s $STOP_SIGNAL $1 > /dev/null
}

function stop_named_container {
  if named_container_running $1; then
    gracefully_stop_container $1

    STOP_TIMEOUT=${STOP_TIMEOUT:-10}
    echo "Waiting max $STOP_TIMEOUT sec for container $1 to stop"
    retry=1
    while named_container_running $1 && [ $retry -lt $STOP_TIMEOUT ]; do
      retry=$((retry+1))
      sleep 1
    done

    if named_container_running $1; then
      echo "Forcefully terminating container $1"
      docker kill $1 > /dev/null
    else
      echo "Container $1 stopped gracefully"
    fi
  fi

  if [ ! -z "$(docker ps -a --filter=name=$1 | grep -v CONTAINER)" ]; then
    echo "Removing container $1"
    docker rm $1 > /dev/null
  fi
}

function setup_env_init {
  # This function sets up some production specific initialization, such as
  # http proxies, and package mirrors.
  # In addition, we export UID of the current host user to the image. While
  # building the image, we'll create the new user with the same id.
  # Note that this is needed only when we're using a "build" container to
  # produce some artifacts in the mounted volume. In such cases, the container
  # needs to have write permissions on a mounted volume, which may not happen
  # if the docker user id is different from the id of the user running the
  # docker on the host system.

  if [ "$AIR_ENV" = "prod" ]; then
    content=$(cat <<-EOF
      echo "deb http://acmirror.mpi-sws.org/debian jessie main" > /etc/apt/sources.list
      echo "deb http://acmirror.mpi-sws.org/debian jessie-updates main" >> /etc/apt/sources.list
      echo "deb http://acmirror.mpi-sws.org/debian-security jessie/updates main" >> /etc/apt/sources.list

      echo 'export https_proxy=http://acmirror.mpi-sws.org:3128' > /tmp/build_config/proxies.sh
      echo 'export http_proxy=http://acmirror.mpi-sws.org:3128' >> /tmp/build_config/proxies.sh

      echo '#!/usr/bin/env bash' > /tmp/build_config/useradd.sh
      echo 'useradd -u $UID "\$@"' >> /tmp/build_config/useradd.sh
      chmod +x /tmp/build_config/useradd.sh
EOF
    )
  else
    if [ -n "$(which boot2docker)" ]; then
      # Don't use host's $UID, since boot2docker ensures proper permissions
      content=$(cat <<-EOF
        echo '#!/usr/bin/env bash' > /tmp/build_config/useradd.sh
        echo 'useradd "\$@"' >> /tmp/build_config/useradd.sh
        chmod +x /tmp/build_config/useradd.sh
EOF
      )
    else
      # No boot2docker -> use host's $UID to ensure proper permissions
      content=$(cat <<-EOF
        echo '#!/usr/bin/env bash' > /tmp/build_config/useradd.sh
        echo 'useradd -u $UID "\$@"' >> /tmp/build_config/useradd.sh
        chmod +x /tmp/build_config/useradd.sh
EOF
      )
    fi
  fi

  if [ -f ./image_shell_init.sh ]; then
    current_content=$(cat ./image_shell_init.sh)
  fi

  # Replace the file only if the content has changed. Otherwise, we leave the file as
  # it is, which will allow the docker to reuse the intermediate image layer.
  if [ "$content" != "$current_content" ]; then
    echo "$content" > ./image_shell_init.sh
  fi
}

function container_ctl {
  container_name=$1
  shift

  command=$1
  shift

  if [ "$AIR_ENV" = "prod" ]; then
    driver_arg="--log-driver=syslog"
    container_env="-e AIR_ENV=prod"
  fi

  case "$command" in
    start)
      stop_named_container $container_name
      echo "Starting container $container_name"
      docker run -d $driver_arg $container_env --restart on-failure --name $container_name $DOCKER_START_ARGS
      ;;

    ensure_started)
      if ! named_container_running $container_name ; then
        # Still invoking stop, since we might need to remove the container
        stop_named_container $container_name
        echo "Starting container $container_name"
        docker run -d $driver_arg $container_env --restart on-failure --name $container_name $DOCKER_START_ARGS
      fi
      ;;

    console)
      stop_named_container $container_name
      docker run --rm -it $container_env --name $container_name $DOCKER_START_ARGS
      ;;

    foreground)
      stop_named_container $container_name
      docker run --rm -i $container_env --name $container_name $DOCKER_START_ARGS
      ;;

    ssh)
      docker exec -i -t $container_name \
        /bin/bash -c "TERM=xterm /bin/bash"
      ;;

    remote_console)
      docker exec -i -t $container_name \
        /bin/bash -c "TERM=xterm $REMOTE_CONSOLE_COMMAND"
      ;;

    stop)
      stop_named_container $container_name
      exit 0
      ;;

    *)
      echo "$(basename $0) start|stop|ensure_started|ssh|remote_console|console|foreground docker-args"
      exit 1
      ;;

  esac
}
