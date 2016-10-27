Central db
============

----------------------

- [What it does](#what-it-does)
- [Running](#running)
- [Publishing the image](#publishing-the-image)

----------------------

## What it does

This component is the containerized central database server. Locally it is used for development and testing. The container can be also used in production as a simple way of hosting the central database.

## Running

In local development, there's no need to start this container manually. The two containers (development and test database servers) are started from the [start_dependencies](../../start_dependencies.sh) script.

## Publishing the image

Running `./publish.sh aircloak` will build the new version of the image and push it to [quay.io](quay.io). The build process takes place on the build server using your current local branch.
