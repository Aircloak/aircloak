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

function container_ctl {
  command=$1

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
        container_ctl $CONTAINER_NAME start "$@"
      fi
      ;;

    ensure_latest_version_started)
      if latest_version_running $CONTAINER_NAME; then
        echo "$CONTAINER_NAME already on the latest image version"
      else
        echo "Restarting $CONTAINER_NAME to ensure that the latest version is running."
        container_ctl $CONTAINER_NAME start "$@"
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
  stop_named_container $CONTAINER_NAME
  echo "Starting container $CONTAINER_NAME"
  docker run $DOCKER_START_ARGS --name $CONTAINER_NAME $DOCKER_IMAGE $CONTAINER_ARGS
}

# Just like build_aircloak_image, but also versions the image and pushes it to the
# registry.
function build_production_image {
  build_aircloak_image "$@"
  push_to_registry $1
  version_latest_image $1
}

# Syntax:
#   build_aircloak_image image_tag dockerfile_path [dockerfileignore_path]
# Note: image build is always running in the repo top folder. Provided paths
# must therefore be relative to the top folder.
function build_aircloak_image {
  curdir=$(pwd)
  cd "$(dirname ${BASH_SOURCE[0]})/.."

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

  temp_docker_file="tmp/$(uuidgen).dockerfile"
  {
    mkdir -p tmp
    echo "[aircloak] building aircloak/$1"
    cat $dockerfile | dockerfile_content > "$temp_docker_file"
    docker build -t aircloak/$1:latest -f "$temp_docker_file" .
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
  cd $curdir
}

# Produces the final dockerfile contents.
# We use some custom templating, where some of our pseudo-commands are replaced
# with actual docker commands.
function dockerfile_content {
  while read line; do
    case "$(echo "$line" | xargs -0)" in
      # production specific initialization of proxies and user ids
      AIR_INIT)
        echo "$(air_init)"
        ;;

      # Version tagging. When a version is bumped, this will cause a new image
      # to be rebuilt. Usually, this instruction should be included at the end
      # of the Dockerfile to reduce the amount of rebuilt layers.
      AIR_TAG_VERSION)
        echo "COPY VERSION /tmp/"
        ;;

      *)
        echo $line
        ;;
    esac
  done
}

function air_init {
  # This function generates the RUN command which sets up some production specific
  # initialization, such as http proxies, and package mirrors.
  # In addition, we export UID of the current host user to the image. While
  # building the image, we'll create the new user with the same id.
  # Note that this is needed only when we're using a "build" container to
  # produce some artifacts in the mounted volume. In such cases, the container
  # needs to have write permissions on a mounted volume, which may not happen
  # if the docker user id is different from the id of the user running the
  # docker on the host system.

  # Start the RUN command
  echo "RUN mkdir -p /tmp/build_config && \\"

  if [ "$AIR_ENV" = "prod" ]; then
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

  # Generate special useradd helper which ensures proper UID on Linux hosts
  if [ "$(which boot2docker)" == "" ]; then
    # No boot2docker -> use host's $UID to ensure proper permissions
    uid_arg="-u $UID"
  fi

  cat <<-EOF
    echo '#!/usr/bin/env bash' > /tmp/build_config/useradd.sh && \\
    echo 'useradd $uid_arg "\$@"' >> /tmp/build_config/useradd.sh && \\
    chmod +x /tmp/build_config/useradd.sh && \\
EOF

  # Support for setting custom prompt through CONTAINER_NAME env. This allows us
  # to inject the container name into the ssh session. Without this, the name of the
  # real host machine will be used, since containers run in the shared networking mode.
  cat <<-EOF
    echo 'PS1="\${debian_chroot:+(\$debian_chroot)}\\\\u@\${CONTAINER_NAME:-\\\\h}:\\\\w# "' \\
      >> /etc/profile.d/custom_prompt.sh
EOF
}

function version_latest_image {
  image_name="aircloak/$1"
  latest_image_id=$(find_images $image_name ^latest$)
  if [ "$latest_image_id" == "" ]; then
    echo "Can't find latest image for $image_name"
    return 1
  fi

  major_minor=$(cat $(dirname ${BASH_SOURCE[0]})/../VERSION)

  last_patch_version=$(
        find_images $image_name ^$major_minor.[0-9]+$ version |
            sed "s/$major_minor\.//" |
            sort -r -n |
            head -n 1
      )
  latest_version_image_id=$(find_images $image_name ^$major_minor.$last_patch_version$)

  if [ "$latest_version_image_id" == "$latest_image_id" ]; then
    echo "No changes for $image_name"
    return 0
  fi

  # Tag with major.minor.last_patch+1
  # If there's no patch version for current major.minor (as defined in VERSION file),
  # we'll tag with major.minor.0
  patch_version=$((${last_patch_version:-"-1"} + 1))
  tag="$image_name:$major_minor.$patch_version"
  echo "Tagging $latest_image_id with $tag"
  docker tag $latest_image_id $tag

  # Keep only last 5 versions.
  for old_version in $(
      find_images $image_name "^[0-9]+\.[0-9]+\.[0-9]+$" version |
          # sort by version descending (using numeric comparison for each field)
          sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
          # skip 5 most recent versions
          tail -n+6
      )
  do
    docker rmi "$image_name:$old_version"
  done
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

function print_most_recent_versions {
  all_images=$(docker images --no-trunc)
  aircloak_images=$(
        echo "$all_images" |
            awk '{if ($1 ~ /^aircloak\/.+/) print $1}' |
            sort |
            uniq
      )
  printf "\nLatest images versions:\n"
  for aircloak_image in $aircloak_images; do
    latest_version=$(
        ALL_IMAGES="$all_images" find_images $aircloak_image '^[0-9]+\.[0-9]+\.[0-9]+$' version |
            sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
            head -n 1
      )

      if [ "$latest_version" != "" ]; then
        echo "$all_images" | \
            awk "{if (\$1 == \"$aircloak_image\" && \$2 == \"$latest_version\") {\$3=\"\"; print;}}"
      fi
  done
  printf "\n"
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
