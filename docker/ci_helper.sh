. docker/docker_helper.sh

COMPONENT=$1
DOCKER_IMAGE="ci_$COMPONENT"
MOUNTS=""
DOCKER_ARGS="$DOCKER_ARGS"

function push_docker_arg {
  DOCKER_ARGS="$1 $DOCKER_ARGS"
}

function mount {
  local source=$1
  local target=$2
  MOUNTS="$MOUNTS -v $source:$target"
}

function mount_to_aircloak {
  for mounted in $@; do
    mount $(pwd)/$mounted /aircloak/$mounted
  done
}

function mount_to_component {
  for mounted in $@; do
    mount $(pwd)/$COMPONENT/$mounted /aircloak/$COMPONENT/$mounted
  done
}

function mount_cached_component {
  for mounted in $@; do
    mount $(ci_tmp_folder)/$COMPONENT/$mounted /aircloak/$COMPONENT/$mounted
  done
}

function ci_tmp_folder {
  component_tmp_folder "ci"
}

function build_component_image {
  # We won't remove old images here, since we're building one more image at the end. That final build will lead to
  # removal of old stale images. Since removal of stale images also talks to git, this will reduce the network
  # communication and will run much faster.

  PREVENT_OLD_IMAGE_REMOVAL="true" build_aircloak_image $DOCKER_IMAGE $COMPONENT/ci/dockerfile $COMPONENT/ci/.dockerignore
  remove_old_git_head_image_tags "aircloak"
}

function build_base_images {
  # build leaf base images (this will cause all parent images to build as well)
  PREVENT_OLD_IMAGE_REMOVAL="true" common/docker/phoenix/build-image.sh
  PREVENT_OLD_IMAGE_REMOVAL="true" common/docker/rust/build-image.sh
}

function is_image_built {
  if [ "$(docker images -q aircloak/$DOCKER_IMAGE:$(git_head_image_tag))" == "" ]; then
    echo "no"
  else
    echo "yes"
  fi
}

function start_container {
  container_name=$1
  local mounts="-v $(ci_tmp_folder)/$COMPONENT/.bash_history:/root/.bash_history $MOUNTS"
  docker network create --driver bridge $container_name > /dev/null

  # need to use eval, to properly escape everything
  local full_docker_args="-d --name $container_name --network=$container_name $mounts $DOCKER_ARGS -e CI=true"
  local stop_after=${STOP_AFTER:-3600}
  full_cmd="docker run $full_docker_args aircloak/$DOCKER_IMAGE:$(git_head_image_tag) sleep $stop_after > /dev/null"
  eval $full_cmd
}

function run_in_container {
  container_name=$1
  shift || true

  # need to use eval, to properly escape everything
  full_cmd="docker exec $DOCKER_ARGS -i -e CI=true $container_name /bin/bash -c \". ~/.asdf/asdf.sh && $@\""
  eval $full_cmd
}

function default_handle {
  command=$1
  shift || true

  case "$command" in
    build_image)
      build_component_image
      ;;

    is_image_built)
      is_image_built
      ;;

    start_container)
      start_container $@
      ;;

    run_in_container)
      run_in_container $@
      ;;

    *)
      echo "invalid args: $command $@"
      exit 1
      ;;
  esac
}

# Dummy execution to verify that tools_versions_md5 returns without an error. We need to do this because the result of
# this function is used only in command interpolations, and the non-zero exit code is swallowed in such cases.
tools_versions_md5 > /dev/null
