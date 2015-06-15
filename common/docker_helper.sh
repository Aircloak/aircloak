#!/usr/bin/env bash


function stop_named_container {
  if [ ! -z "$(docker ps --filter=name=$1 | grep -v CONTAINER)" ]; then
    echo "Stopping container $1"
    docker stop $1 > /dev/null
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

  case "$command" in
    start)
      stop_named_container $container_name
      echo "Starting container $container_name"
      docker run -d --restart on-failure --name $container_name "$@"
      ;;

    ensure_started)
      RUNNING=$(docker inspect --format="{{ .State.Running }}" $container_name || echo false)
      if [ "$RUNNING" != "true" ]; then
        echo "Starting container $container_name"
        docker run -d --restart on-failure --name $container_name "$@"
      fi
      ;;

    console)
      stop_named_container $container_name
      docker run --rm -it --name $container_name "$@"
      ;;

    remsh)
      docker exec -i -t $container_name /bin/bash
      ;;

    stop)
      stop_named_container $container_name
      exit 0
      ;;

    *)
      echo "$(basename $0) start|stop|ensure_started|remsh|console docker-args"
      exit 1
      ;;

  esac
}
