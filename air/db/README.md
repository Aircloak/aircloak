Air db
============

----------------------

- [What it does](#what-it-does)
- [Running](#running)
- [Publishing the image](#publishing-the-image)

----------------------

## What it does

This component is the containerized air database server. Locally it is used for development and testing. The container can be also used in production as a simple way of hosting the air database.

## Running

In local development, there's no need to start this container manually. The two containers (development and test database servers) are started from the [start_dependencies](../start_dependencies.sh) script.

__OS X developers__: You should be aware that your database data is stored inside docker-machine VM in
`/mnt/sda1/docker_volumes/air_db_*` folders. This data persists after restarts of docker-machine (or the host itself). However, __if you delete docker-machine VM, this data will be lost.__

## Publishing the image

Running `./publish.sh aircloak` will build the new version of the image and push it to [quay.io](quay.io). The build process takes place on the build server using your current local branch.
