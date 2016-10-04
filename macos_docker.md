On macOS, we're using Docker for Mac, so some additional setting up is needed. If you've been previously running docker-machine, see [below](#migrating-from-docker-machine).

1. Once Docker for Mac is installed, you need to create the `/docker_volumes` folder by running `sudo mkdir -p /docker_volumes && sudo chown $USER /docker_volumes/`.
1. Then in your Docker for Mac settings you need to share this project folder and `/docker_volumes`.
1. Build the images by running `air/db/build.sh`, `air/build.sh`, and `cloak/build.sh`.
1. Start air dependencies with `air/start_dependencies.sh`.
1. Create the air database: `cd air && make recreate_db`.
1. Create the cloak container database: `cd cloak && DB_PORT=20002 ./regenerate_db.sh`

If all went well, you will be able to run air tests and start local air (both use the containerized database). To test containerized air and cloak, run `air/container.sh console` followed by `cloak/container.sh console`.


## Migrating from docker-machine

1. Stop your docker containers, and docker machine
2. Comment out `eval $(docker-machine env default)` or any other DOCKER exports from `.bash_profile`, then start the new terminal session.
3. Install Docker for Mac (see [here](https://docs.docker.com/docker-for-mac/) for instructions) and start it. __Caveat__: I had some issues when migrating images from docker-machine. Docker was constantly blocked and could not boot. I did a factory reset in Docker for Mac UI, skipped the migration step, and then everything worked fine.

