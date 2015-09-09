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

  mkdir -p tmp
  if [ -f tmp/image_shell_init.sh ]; then
    current_content=$(cat tmp/image_shell_init.sh)
  fi

  # Replace the file only if the content has changed. Otherwise, we leave the file as
  # it is, which will allow the docker to reuse the intermediate image layer.
  if [ "$content" != "$current_content" ]; then
    echo "$content" > tmp/image_shell_init.sh
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
        container_ctl $container_name start "$@"
      fi
      ;;

    ensure_latest_version_started)
      if ! named_container_running $container_name ; then
        container_ctl $container_name start "$@"
      else
        image_id=$(docker inspect -f="{{.Image}}" $container_name)
        image_name=$(docker images --no-trunc |
              awk "{if (\$3 == \"$image_id\" && \$1 ~ /^aircloak\/.+$/) print \$1}" |
              sort |
              uniq
            )
        most_recent_image_id=$(find_images $image_name ^latest$)

        if [ "$image_id" == "$most_recent_image_id" ]; then
          echo "$container_name already on the latest image version"
        else
          echo "Restarting $container_name to ensure that the latest version is running."
          container_ctl $container_name start "$@"
        fi
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
      echo "$(basename $0) start|stop|ensure_started|ensure_latest_version_started|ssh|remote_console|console|foreground docker-args"
      exit 1
      ;;

  esac
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

  {
    echo "[aircloak] building aircloak/$1"
    setup_env_init
    docker build -t aircloak/$1:latest -f "$dockerfile" .
  } || {
    # called in the case of an error
    exit_code=$?
    if [ -f .dockerignore ]; then rm .dockerignore; fi
    cd $curdir
    exit $exit_code
  }

  if [ -f .dockerignore ]; then rm .dockerignore; fi
  cd $curdir
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
