This is a dockerized PostgreSQL database used in performance tests on `diffix0.mpi-sws.org`.

## Running

You can build the local image with `./build-image.sh`. Then you can start it with the `./container.sh console`.

## Publishing the image

Running `./publish.sh` will build the new version of the image locally and push it to [quay.io](quay.io).
In order for the image to be successfully pushed to quay it requires that you have authorised your computer
with write privileges to the performance_db repository on quay.
