#!/bin/bash

set -eo pipefail

build_folder="/home/ci/aircloak_ci/build/ci"
production_folder="/home/ci/aircloak_ci/production"

function exec_as_root {
  ssh acatlas4.mpi-sws.org "$@"
}

function exec_as_ci {
  exec_as_root "su ci -c \"$(set_ci_env) && . /home/ci/.asdf/asdf.sh && $@\""
}

function set_ci_env {
  printf "export https_proxy=http://acmirror.mpi-sws.org:3128 &&"
  printf "export http_proxy=http://acmirror.mpi-sws.org:3128 &&"
  printf "export no_proxy=localhost,127.0.0.1,acmirror.mpi-sws.org,aclog0.mpi-sws.org"
}

function exec_on_build {
  exec_as_ci "cd $build_folder && $@"
}

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

function deploy {
  build_branch=$(git symbolic-ref --short HEAD)
  if [ "$build_branch" != "master" ]; then
    echo "Warning: deploying from branch $build_branch"
    read -p "Continue (y/N)? " -r
    if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
  fi

  git show-ref --verify --quiet refs/remotes/origin/$build_branch || {
    echo  "The branch $build_branch must be pushed to the remote first!"
    exit 1
  }

  release_name=$(date "+%Y%m%d%H%M%S")

  exec_on_build "
    set -eo pipefail

    # grab the build lock
    $(lock_command "ci_build")

    # update local repo and checkout the target branch
    git reset --hard HEAD
    git fetch
    git checkout $build_branch
    git reset --hard origin/$build_branch

    # build release and make the current folder point to it
    make release
    mkdir -p $production_folder/releases/
    mv _build/prod/rel/aircloak_ci $production_folder/releases/$release_name
    ln -nfs $production_folder/releases/$release_name $production_folder/current

    # delete obsolete local branches (the ones not existing on remote)
    git remote prune origin
    (git branch -vv | grep ': gone]' | awk '{print $1}' | xargs git branch -d &>/dev/null || true)
  "

  # update systemd service file and restart the service
  exec_as_root "
    cp -rp $build_folder/production/aircloak_ci.service /etc/systemd/system/ &&
    systemctl daemon-reload &&
    systemctl restart aircloak_ci.service
  "

  # keep the most recent 10 releases
  if [ "$(exec_as_ci "cd $production_folder/releases && ls -tp | tail -n +11")" != "" ]; then
    exec_as_ci "cd $production_folder/releases && ls -tp | tail -n +11 | xargs rm -rf"
  fi

  echo "CI server has been deployed successfully"
}

function rollback {
  echo "Are you sure you want to perform a rollback?"
  read -p "Continue (y/N)? " -r
  if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi

  current_release=$(exec_as_ci "basename \$(readlink $production_folder/current)")
  previous_release=$(exec_as_ci "ls -t1 $production_folder/releases | grep -A 1 $current_release | tail -n 1")
  if [ "$previous_release" != "" ]; then
    exec_on_build "ln -nfs $production_folder/releases/$previous_release $production_folder/current"
    exec_as_root "systemctl restart aircloak_ci.service"
  fi

  echo "rolled back to $previous_release"
}

function service_command {
  exec_as_ci "$production_folder/current/bin/aircloak_ci $@"
}

command="$1"
shift || true

case "$command" in
  deploy)
    deploy
    ;;

  rollback)
    rollback
    ;;

  service_log)
    exec_as_root "journalctl -u aircloak_ci.service --no-pager $@"
    ;;

  build_log)
    service_command build log $1
    ;;

  force_pr_build)
    service_command build force_pr_build $1
    ;;

  force_branch_build)
    service_command build force_branch_build $1
    ;;

  *)
    echo "Usage: ./$(basename "$0") deploy | rollback | service_log journalctl_args | build_log pr_number | force_pr_build pr_number"
    exit 1
    ;;
esac
