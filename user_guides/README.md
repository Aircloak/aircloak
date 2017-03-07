# UserGuides

This project is used to build shippable html guides for our clients. In most cases a more appropriate place for guides is inside the `air` project. However, in some cases (e.g. installation guides), this is not possible, so you can use this project instead.


## Releasing the guides

You can release the guides by running `make release`. This will create the `/tmp/aircloak_guides-X.Y.Z.tar.gz` tarball, which you can e-mail directly to clients.


## Local development

You'll need to run `make` once, which will fetch dependencies and build the guides in the `_build/guides` folder. Then, you need to open the HTML file in your browser. Now, you can modify sources in the `source` folder. Keep in mind that you need to rebuild HTML on every change using `make` or `make compile` (the latter doesn't refetch the dependencies).
