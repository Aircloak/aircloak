#!/usr/bin/env bash

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

function container_ctl {
  command=$1
  shift || true

  case "$command" in
    start)
      DOCKER_START_ARGS="$DOCKER_START_ARGS -d $driver_arg --restart on-failure"
      start_container
      ;;

    console)
      DOCKER_START_ARGS="--rm -it $DOCKER_START_ARGS"
      start_container
      ;;

    foreground)
      DOCKER_START_ARGS="--rm -i $DOCKER_START_ARGS"
      start_container
      ;;

    ensure_started)
      if ! named_container_running $CONTAINER_NAME ; then
        container_ctl start $@
      fi
      ;;

    ssh)
      docker exec -i -t $CONTAINER_NAME \
        /bin/bash -c "TERM=xterm CONTAINER_NAME='$CONTAINER_NAME' /bin/bash -l"
      ;;

    remote_console)
      docker exec -i -t $CONTAINER_NAME \
        /bin/bash -c "TERM=xterm ${REMOTE_CONSOLE_COMMAND:-"/bin/bash"}"
      ;;

    stop)
      stop_named_container $CONTAINER_NAME
      exit 0
      ;;

    image_name)
      echo "$DOCKER_IMAGE"
      ;;

    *)
      commands="
        start - starts the container in background (restarts the running container)
        stop - stops the container if it is running
        ensure_started - starts the container in background if it is not running
        ensure_latest_version_started - starts the container in background if it is not running the latest version
        ssh - opens a terminal session in the running container
        remote_console - opens an application session (e.g. Erlang or Rails console) in the running container
        console - starts the container in foreground (interactive)
        foreground - starts the container in foreground (non-interactive)
        image_name - prints the image name
        $CUSTOM_COMMANDS
      "

      printf "\nUsage: $0 command\n\n"
      while read line; do
        if [ "$line" != "" ]; then
          echo $line | awk -F ' - ' '{printf "%-35s%s\n", $1, $2}'
        fi
      done < <(echo "$commands" | sort)
      echo
      exit 1
      ;;

  esac
}

function start_container {
  if [ "$REGISTRY_URL" != "" ]; then DOCKER_IMAGE="$REGISTRY_URL/$DOCKER_IMAGE"; fi

  if [ "$DOCKER_IMAGE_VERSION" != "" ]; then
    DOCKER_IMAGE="$DOCKER_IMAGE:$DOCKER_IMAGE_VERSION"
  fi

  stop_named_container $CONTAINER_NAME
  echo "Starting container $CONTAINER_NAME from $DOCKER_IMAGE"
  docker run $DOCKER_START_ARGS --name $CONTAINER_NAME $DOCKER_IMAGE $CONTAINER_ARGS
}

# Syntax:
#   build_aircloak_image image_tag dockerfile_path [dockerfileignore_path]
# Note: It is the responsibility of the caller to properly set the current folder.
function build_aircloak_image {
  curdir=$(pwd)

  dockerfile="$2"
  if [ -d $dockerfile ]; then
    dockerfile="$dockerfile/Dockerfile"
  fi

  dockerignore_file=${3:-"$(dirname $dockerfile)/.dockerignore"}
  if [ -f $dockerignore_file ]; then
    cp -rp $dockerignore_file .dockerignore
  else
    if [ -f .dockerignore ]; then rm .dockerignore; fi
  fi

  full_image_name=$(aircloak_image_name $1)

  temp_docker_file="tmp/$(uuidgen).dockerfile"
  {
    mkdir -p tmp
    echo "[aircloak] building $full_image_name"
    cat $dockerfile | dockerfile_content > "$temp_docker_file"
    docker build -t $full_image_name:latest -f "$temp_docker_file" .
  } || {
    # called in the case of an error
    exit_code=$?
    if [ -f "$temp_docker_file" ]; then rm "$temp_docker_file"; fi
    if [ -f .dockerignore ]; then rm .dockerignore; fi
    cd $curdir
    exit $exit_code
  }

  if [ -f "$temp_docker_file" ]; then rm "$temp_docker_file"; fi
  if [ -f .dockerignore ]; then rm .dockerignore; fi

  # remove exited containers
  exited_containers=$(docker ps --quiet -f status=exited)
  if [ "$exited_containers" != "" ]; then docker rm $exited_containers || true; fi

  # remove local version tags (obsolete in the new version of the build system)
  local_version_tags=$(docker images | awk "{if (\$1 == \"$full_image_name\" && \$2 != \"latest\") print \$1\":\"\$2}")
  if [ "$local_version_tags" != "" ]; then docker rmi $local_version_tags || true; fi

  # remove dangling images
  dangling_images=$(docker images --quiet --filter "dangling=true")
  if [ "$dangling_images" != "" ]; then docker rmi $dangling_images || true; fi

  cd $curdir
}

function aircloak_image_name {
  if [ "$IMAGE_CATEGORY" == "" ]; then
    image_name="$1"
  else
    image_name="${IMAGE_CATEGORY}_$1"
  fi

  echo "aircloak/$image_name"
}

# Produces the final dockerfile contents.
# We use some custom templating, where some of our pseudo-commands are replaced
# with actual docker commands.
function dockerfile_content {
  while read line; do
    case "$(echo "$line" | xargs -0)" in
      # production specific initialization of proxies and user ids
      MPI_INIT)
        echo "$(mpi_init)"
        ;;

      # Version tagging. When a version is bumped, this will cause a new image
      # to be rebuilt. Usually, this instruction should be included at the end
      # of the Dockerfile to reduce the amount of rebuilt layers.
      TAG_VERSION)
        echo "RUN echo '$SYSTEM_VERSION' > /tmp/VERSION"
        ;;

      *)
        echo $line
        ;;
    esac
  done
}

function mpi_init {
  # This function generates the RUN command which sets up some production specific
  # initialization, such as http proxies, and package mirrors.
  # In addition, we export UID of the current host user to the image. While
  # building the image, we'll create the new user with the same id.
  # Note that this is needed only when we're using a "build" container to
  # produce some artifacts in the mounted volume. In such cases, the container
  # needs to have write permissions on a mounted volume, which may not happen
  # if the docker user id is different from the id of the user running the
  # docker on the host system.

  # Support for rebuilding of all images. Change this date if you want to
  # force the rebuild of all images. Usually you want to do this if you
  # want to upgrade Debian packages on all images.
  upgrade_date="20160302"
  echo "RUN echo '$upgrade_date' > /dev/null"

  # Start the RUN command
  echo "RUN mkdir -p /tmp/build_config && \\"

  if [ "$CONTAINER_ENV" = "prod" ]; then
    # set mpi repos & proxies
    cat <<-EOF
      echo "deb http://acmirror.mpi-sws.org/debian jessie main" > /etc/apt/sources.list && \\
      echo "deb http://acmirror.mpi-sws.org/debian jessie-updates main" >> /etc/apt/sources.list && \\
      echo "deb http://acmirror.mpi-sws.org/debian-security jessie/updates main" >> /etc/apt/sources.list && \\
      echo 'export https_proxy=http://acmirror.mpi-sws.org:3128' > /tmp/build_config/proxies.sh && \\
      echo 'export http_proxy=http://acmirror.mpi-sws.org:3128' >> /tmp/build_config/proxies.sh && \\
EOF
  else
    # local development -> generate dummy proxies.sh
    echo "touch /tmp/build_config/proxies.sh && \\"
  fi

  # Upgrade existing packages
  echo "apt-get update && apt-get upgrade -y && \\ "

  # Support for setting custom prompt through CONTAINER_NAME env. This allows us
  # to inject the container name into the ssh session. Without this, the name of the
  # real host machine will be used, since containers run in the shared networking mode.
  cat <<-EOF
    echo 'PS1="\${debian_chroot:+(\$debian_chroot)}\\\\u@\${CONTAINER_NAME:-\\\\h}:\\\\w# "' \\
      >> /etc/profile.d/custom_prompt.sh
EOF
}

function find_images {
  case "$3" in
    "" | id) print_column='$3' ;; # print image id by default
    version) print_column='$2' ;;
    *)
      echo "Invalid column specifier $3" >&2
      return 1
    ;;
  esac

  all_images=${ALL_IMAGES:-"$(docker images --no-trunc)"}

  echo "$all_images" | \
      awk "{if (\$1 == \"$1\" && \$2 ~ /${2:-".*"}/) print $print_column}" | \
      sort | \
      uniq
}

function latest_version_running {
  if named_container_running $1; then
    image_id=$(docker inspect -f="{{.Image}}" $1)
    image_name=$(docker images --no-trunc |
          awk "{if (\$3 == \"$image_id\" && \$1 ~ /^aircloak\/.+$/) print \$1}" |
          sort |
          uniq
        )
    most_recent_image_id=$(find_images $image_name ^latest$)

    if [ "$image_id" == "$most_recent_image_id" ]; then return 0; fi
  fi

  return 1
}

function cleanup_unused_images {
  stopped_containers=$(docker ps -a | grep -v Up | awk '{print $1}' | tail -n+2)
  if [ "$stopped_containers" != "" ]; then docker rm $(echo "$stopped_containers"); fi

  unused_images=$(docker images -q -f dangling=true)
  if [ "$unused_images" != "" ]; then docker rmi $(echo "$unused_images"); fi
}
