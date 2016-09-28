This document serves to describe breaking changes and provide upgrade hints when major changes are introduced. When you're creating a pull with some major changes, please add brief upgrade instructions here.

## Standalone air

- Air web project now resides directly in the `air` folder.
- Air listens on ports 8080 and 8443.
- Change in deploy syntax: use `air/production.sh target deploy` or `cloak/production.sh target deploy`, where `target` is your first name (without paths).
- New command to deploy air and cloak together: `./publish.sh target`.
- Air is deployed to srv-76-135.
- Following components are not used anymore: `nginx`, `haproxy`, `etcd`, `coreos`.

## Main branch change from `develop` to `master`

The change was made to get Travis to pick up our caches, and secondly it's what most people and projects expect. Update your local git remotes.

## Mixification of cloak

- Cloak project is now compiled with `mix` and powered by Elixir. You'll need Erlang 18.x and Elixir >= 1.2.3 to run it. There is the `Makefile` file in the `cloak` folder with typical tasks.
- Extended proper tests (full, batch) are currently not working, and they are excluded from Travis tests.

## Dockerization of insights

- The role which accesses the test database has been changed, so your local tests will probably break. To fix this, start the docker database container, drop `air_test` database, and finally restart the database container.

## Running air (insights) through the router

- New pseudo local site has been introduced: `insights.air-local`. You need to add the corresponding entry to your `/etc/hosts`.
- The certificate in `air/router/dev_cert/aircloak.com.chain.pem` has been changed. You need to delete the old key from your keychain, and import the new version as explained [here](air/README.md#running-the-system-on-the-localhost).
- The new site is now accessed through the router. You need to start air dependencies, and then the site will be available at https://insights.air-local:20000
