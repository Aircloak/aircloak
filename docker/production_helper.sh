function lock_command {
  printf "
    echo 'Acquiring lock for $1'
    if lockfile -1 -r 120 /tmp/$1; then
      trap '{ rm -f /tmp/$1; }' EXIT
    else
      echo 'Could not acquire lock for $1.'
      exit 1
    fi
  "
}

function image_name {
  IMAGE_CATEGORY="$DEPLOYMENT_NAME" ./container.sh image_name
}

function build_folder {
  echo "/aircloak/quay_deploy/aircloak/$1"
}

function build_branch {
  git symbolic-ref --short HEAD
}

function build_image {
  ssh acdbuild.mpi-sws.org "
    set -eo pipefail

    $(lock_command "build_$1")

    echo 'Pulling the latest version'
    cd $(build_folder $2)
    git reset --hard HEAD
    git fetch
    git checkout $(build_branch)
    git reset --hard origin/$(build_branch)
    echo 'Building the image'
    CONTAINER_ENV=prod IMAGE_CATEGORY=$DEPLOYMENT_NAME PERFORM_VERSION_CHECK=$PERFORM_VERSION_CHECK $(build_folder $2)/package.sh
  "
}

function start_component {
  start_at_version $1 "$(image_name):$2"
}

function start_at_version {
  container_name=$1
  full_image_name="quay.io/$2"
  ssh $TARGET_MACHINE "
    set -eo pipefail

    $(lock_command $container_name)

    docker pull $full_image_name

    echo 'Stopping container $container_name'
    docker stop $container_name || true
    docker rm $container_name || true

    echo 'Starting container $container_name'
    docker run -d -t \\
      --name $container_name \\
      -h ${container_name//_/-} \\
      -v $RUNTIME_CONFIG_PATH:/runtime_config \\
      $DOCKER_ARGS \\
      $full_image_name
  "
}

function print_usage {
  echo
  echo "Usage:"
  echo "  $0 deploy_configuration deploy"
  echo "  $0 deploy_configuration versions"
  echo "  $0 deploy_configuration rollback version"
  echo
}

function run_production_command {
  component_name="$1"
  shift

  component_folder="$1"
  shift

  container_name="$1"
  shift

  if [ $# -lt 1 ]; then
    print_usage
    exit 1
  fi

  case "$1" in
    versions)
      ssh acdbuild.mpi-sws.org "
        . /aircloak/quay_deploy/aircloak/docker/docker_helper.sh &&
        published_image_versions $(image_name)
      "
      ;;

    rollback)
      start_component $container_name $2
      ;;

    publish)
      if [ "$SKIP_BRANCH_CHECK" != "true" ] && [ "$(build_branch)" != "master" ]; then
        echo "Warning: deploying from branch $(build_branch)"
        read -p "Continue (y/N)? " -r
        if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
      fi

      build_image $component_name $component_folder
      ;;

    deploy)
      if [ "$SKIP_BRANCH_CHECK" != "true" ] && [ "$(build_branch)" != "master" ]; then
        echo "Warning: deploying from branch $(build_branch)"
        read -p "Continue (y/N)? " -r
        if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
      fi

      build_image $component_name $component_folder
      start_component $container_name latest
      ;;

    start_at_version)
      start_at_version $container_name $2
      ;;

    *)
      print_usage
      exit 1
      ;;
  esac
}

# load deploy configuration
if [ "$1" == "" ]; then
  print_usage
  exit 1
fi

. "$(dirname ${BASH_SOURCE[0]})/../deploy_targets/$1"
shift
