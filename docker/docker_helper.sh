#!/usr/bin/env bash

function named_container_running {
  if [ -z "$(docker ps --filter=name=$1 | grep -w $1)" ]; then
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

  if [ ! -z "$(docker ps -a --filter=name=$1 | grep -w $1)" ]; then
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
  docker run $DOCKER_START_ARGS --name $CONTAINER_NAME -h ${CONTAINER_NAME//_/-} $DOCKER_IMAGE $CONTAINER_ARGS
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
    cat $dockerignore_file |
      sed "s#\\\$CLOAK_CACHE#$(cloak_cache_folder)#" > .dockerignore
  else
    if [ -f .dockerignore ]; then rm .dockerignore; fi
  fi

  image_version=${4:-"latest"}
  full_image_name=$(aircloak_image_name $1)
  temp_docker_file="tmp/$(uuidgen).dockerfile"
  build_args="-t $full_image_name:$image_version -f "$temp_docker_file" ."

  if [ "$DOCKER_BUILD_CACHED" == "true" ]; then
    build_args="--cache-from $full_image_name:$image_version $build_args"
  fi

  {
    mkdir -p tmp
    echo "[aircloak] building $full_image_name"
    cat $dockerfile |
      dockerfile_content |
      sed "s|\$RELEASE_VERSION|$SYSTEM_VERSION|" |
      sed "s/\$DEBIAN_VERSION/$(debian_version)/" |
      sed "s/\$ERLANG_VERSION/$(erlang_version)/" |
      sed "s/\$ELIXIR_VERSION/$(elixir_version)/" |
      sed "s/\$RUST_VERSION/$(rust_version)/" |
      sed "s/\$NODEJS_VERSION/$(nodejs_version)/" |
      sed "s#\\\$CLOAK_CACHE#$(cloak_cache_folder)#" > "$temp_docker_file"
    docker build $build_args

    # We'll also tag the image with the current git head sha, and remove all obsolete git_sha_* image tags, which point
    # to an older head. This allows us to keep cached builds for all currently known branches (local and remotes),
    # which significantly reduces the image build time in the cases where there are significant differences between
    # different branches.

    docker tag $full_image_name:$image_version $full_image_name:$(git_head_image_tag)
    remove_old_git_head_image_tags $full_image_name
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

  if [ "$image_version" == "latest" ]; then
    # remove local non-latest version tags (obsolete in the new version of the build system)
    # however, we'll keep git_sha_*, since they are references to the known git heads
    local_version_tags=$(docker images | grep -v git_sha | awk "{if (\$1 == \"$full_image_name\" && \$2 != \"latest\") print \$1\":\"\$2}")
    if [ "$local_version_tags" != "" ]; then docker rmi $local_version_tags || true; fi
  else
    # remove local "latest" version tags
    local_version_tags=$(docker images | awk "{if (\$1 == \"$full_image_name\" && \$2 == \"latest\") print \$1\":\"\$2}")
    if [ "$local_version_tags" != "" ]; then docker rmi $local_version_tags || true; fi
  fi

  # remove dangling images
  dangling_images=$(docker images --quiet --filter "dangling=true")
  if [ "$dangling_images" != "" ]; then docker rmi $dangling_images || true; fi

  cd $curdir
}

function aircloak_image_name {
  # Aircloak images are non prefixed, since we treat them as official
  # production releases.
  if [ "$IMAGE_CATEGORY" == "" ] || [ "$IMAGE_CATEGORY" == "aircloak" ]; then
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

      # We need to configure proxy for yarn in a specific way.
      CONFIG_YARN_PROXY)
        echo "$(config_yarn_proxy)"
        ;;

      *)
        echo $line
        ;;
    esac
  done
}

function config_yarn_proxy {
  if [ "$MPI" == "true" ]; then
    echo 'RUN bash -c ". ~/.asdf/asdf.sh && yarn config set proxy http://acmirror.mpi-sws.org:3128/ && yarn config set https-proxy http://acmirror.mpi-sws.org:3128/"'
  fi
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
  upgrade_date="20171208"
  echo "RUN echo '$upgrade_date' > /dev/null"

  # Start the RUN command
  echo "RUN mkdir -p /tmp/build_config && \\"

  if [ "$CONTAINER_ENV" = "prod" ]; then
    # set repos & proxies
    cat <<-EOF
      echo "deb http://ftp.de.debian.org/debian jessie main" > /etc/apt/sources.list && \\
      echo "deb http://ftp.de.debian.org/debian jessie-updates main" >> /etc/apt/sources.list && \\
      echo "deb http://ftp.de.debian.org/debian-security jessie/updates main" >> /etc/apt/sources.list && \\
      echo 'export https_proxy=http://acmirror.mpi-sws.org:3128' > /tmp/build_config/proxies.sh && \\
      echo 'export http_proxy=http://acmirror.mpi-sws.org:3128' >> /tmp/build_config/proxies.sh && \\
      echo 'export no_proxy=localhost,127.0.0.1,acmirror.mpi-sws.org' >> /tmp/build_config/proxies.sh && \\
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

function check_registry {
  response=$(registry_v2_req "")

  if [ "$response" != "true" ]; then
    printf "\nCan't connect to registry ($REGISTRY_URL): $response\n\n"
    exit 1
  fi
}

# This function builds a docker image and pushes it if needed. The build machine
# keeps the last built image, which allows it to determine whether the image has
# been changed and whether we need to push the new version after the build.
#
# The versioning algorithm is as follows:
#   1. The last version number is determined by the registry
#   2. If the major/minor version has been changed, the image
#      is always pushed to the registry with the version MAJOR.MINOR.0
#   3. If the new image is different from the previously built image,
#      and MAJOR.MINOR versions are unchanged, the patch version is bumped
#      and the image is pushed to the registry.
#   4. Otherwise, there are no changes so nothing is pushed to the registry.
#
# Caveats:
#   - If the build server is reinstalled, or images are deleted locally, next
#     build will always bump the version.
#   - If the last locally built image is not the same as in registry, but is the
#     same as the new built version, we won't push anything. This situation is
#     not very likely, but it's a potential downside of the chosen approach. If
#     you need to force the new version to be pushed, you can delete the local
#     image.
function build_and_push_to_registry {
  new_version=$(verify_version)

  # build new image
  echo "Building $image_name:$new_version"
  ./build-image.sh
  new_image_id=$(find_images $image_name ^latest$)

  echo "Pushing $image_name:$new_version to the registry"
  docker tag "$new_image_id" "quay.io/$image_name:$new_version"
  docker push "quay.io/$image_name:$new_version"

  # also tag with latest
  docker tag "$new_image_id" "quay.io/$image_name:latest"
  docker push "quay.io/$image_name:latest"
}

function verify_version() {
  latest_pushed_version=$(
    registry_v2_req $image_name/tags/list |
    jq --raw-output ".tags | select(. != null) | .[]" |
    sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
    head -n 1
  )

  if [ -f "./VERSION" ]; then
    new_version=$(cat ./VERSION)
  else
    new_version=$(cat ../VERSION)
  fi

  more_recent_version=$(
    printf '%s\n' $new_version $latest_pushed_version |
    sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
    head -n 1
  )

  # Note: we're explicitly testing PERFORM_VERSION_CHECK against false. As a result, this value is by default
  # true if not specified, or even if there's a typo. This is a safer choice, because it reduces a chance of
  # accidentally disabling the version check (which we want to do when deploying to production).
  if [ "$PERFORM_VERSION_CHECK" != "false" ] && [ "$more_recent_version" == "$latest_pushed_version" ]; then
    echo "Can't publish $image_name:$new_version because $latest_pushed_version is already published." >&2
    exit 1
  else
    echo "$new_version"
  fi
}

function registry_v2_req {
  # Authentication as described in https://docs.docker.com/registry/spec/auth/token/#/how-to-authenticate

  # 1. make a request which is bound to fail, but will return auth specification in `WWW-Authenticate`
  authenticate_header=$(
    curl -s -I https://quay.io/v2/$1 |
      grep 'WWW-Authenticate: ' |
      sed 's/WWW-Authenticate: //'
  )

  # 2. Extract auth params
  IFS=" " read auth_method auth_spec < <(echo "$authenticate_header")
  IFS="," read realm auth_params < <(echo "$auth_spec")

  # 3. shape auth url
  auth_params=$(echo "$auth_params" | sed 's/,/\&/g' | sed 's/"//g')
  url="https://quay.io/v2/auth?$auth_params"
  url=${url%$'\r'} # remove trailing \n

  # 4. read user auth token from the secrets folder and use it to fetch operation auth token
  quay_auth_token=$(cat $(dirname ${BASH_SOURCE[0]})/../secrets/quay_auth_token)
  operation_token=$(curl -s -H "Authorization: Basic $quay_auth_token" "$url" | jq --raw-output ".token")

  # 5. Now we can make authorized request
  result=$(curl -s -H "Authorization: $auth_method $operation_token" https://quay.io/v2/$1)
  echo $result
}

function untag_registry_tags {
  # Remove all local repo tags. We don't need those, since the image is tagged
  # anyway, and this allows us proper local cleanup of older images.
  repo_tags=$(docker images | grep "quay.io/$1" | awk '{print $1":"$2}' || true)
  if [ "$repo_tags" != "" ]; then
    docker rmi $repo_tags
  fi
}

function git_head_image_tag {
  # We're tagging the version with the HEAD sha, which reduces collisions with other builds.
  echo "git_sha_$(git rev-parse HEAD)"
}

function debian_version {
  cat "$(dirname ${BASH_SOURCE[0]})/../.debian-version"
}

function erlang_version {
  cat "$(dirname ${BASH_SOURCE[0]})/../.tool-versions" | grep erlang | sed s/'erlang '//
}

function elixir_version {
  cat "$(dirname ${BASH_SOURCE[0]})/../.tool-versions" | grep elixir | sed s/'elixir '//
}

function nodejs_version {
  cat "$(dirname ${BASH_SOURCE[0]})/../.tool-versions" | grep nodejs | sed s/'nodejs '//
}

function rust_version {
  cat "$(dirname ${BASH_SOURCE[0]})/../.tool-versions" | grep rust | sed s/'rust '//
}

function tools_versions_md5 {
  printf "debian $(cat .debian-version)\n$(cat .tool-versions)" | md5sum | awk '{print $1}'
}

function component_tmp_folder {
  echo "$(pwd)/tmp/$1/$(tools_versions_md5)"
}

function cloak_cache_folder {
  folder=$(component_tmp_folder cloak)
  mkdir -p $folder
  realpath --relative-to "." $folder
}

function published_images {
  registry_v2_req _catalog |
    jq --raw-output ".repositories []" |
    grep aircloak |
    sort
}

function published_image_versions {
  for version in $(
    registry_v2_req $1/tags/list |
      jq --raw-output ".tags | select(. != null) | .[]" |
      grep -v latest |
      sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn"
  ); do
    created_at=$(
      registry_v2_req $1/manifests/$version |
        jq --raw-output ".history[0].v1Compatibility" |
        jq --raw-output ".created"
    )
    echo "$version ($created_at)"
  done
}

function package_image {
  if [ "$IMAGE_CATEGORY" == "" ]; then
    echo "Please specify some deploy environment through IMAGE_CATEGORY variable."
    exit 1
  fi

  image_name=$(./container.sh image_name)
  check_registry
  build_and_push_to_registry
  untag_registry_tags "$image_name"
}

function reachable_heads {
  # use all local refs
  echo "$(git show-ref --head)" | awk '{print "git_sha_"$1}'

  # use remote refs
  echo "$(git ls-remote --heads)" | awk '{print "git_sha_"$1}'

  # use remote PR merges
  echo "$(git ls-remote | grep merge)" | awk '{print "git_sha_"$1}'
}

function remove_old_git_head_image_tags {
  if [ "$PREVENT_OLD_IMAGE_REMOVAL" != "true" ]; then
    # For the given image, removes all version tags which do not correspond to the reachable local or remote HEAD,
    # including pending pull requests.

    image=$1
    known_heads=$(reachable_heads | uniq)
    echo "$(docker images | grep $image | awk '{print $1 " " $2}' | grep 'git_sha_')" |
      while IFS=$'\n' read -r existing_image; do
        full_image_name=$(echo $existing_image | awk '{print $1}')
        existing_version=$(echo $existing_image | awk '{print $2}')
        if [[ ! "$known_heads" =~ "$existing_version" ]]; then
          echo "removing image tag for $full_image_name:$existing_version"
          docker rmi $full_image_name:$existing_version || true
        fi
      done
  fi
}
