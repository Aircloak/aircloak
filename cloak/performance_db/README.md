This is a dockerized PostgreSQL database used in performance tests on `diffix0.mpi-sws.org`.

## Running

You can build the local image with `./build-image.sh`. Then you can start it with the `./container.sh console`.

## Publishing the image

Running `./publish.sh aircloak` will build the new version of the image and push it to [quay.io](quay.io). The build process takes place on the build server using your current local branch.
