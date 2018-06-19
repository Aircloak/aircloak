# Central

`central` is an Aircloak hosted application used for management of customers,
recording of Aircloak usage for billing purpose, as well as for collecting usage
information for analytics purposes.


----------------------

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
- [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Running](#running)
    - [Local docker container](#local-docker-container)
    - [Other common tasks](#other-common-tasks)
    - [Deploying](#deploying)

----------------------

## What it does

This repository contains the Central system, which provides HTTPS endpoints that allows Aircloak
to perform various tasks, such as:

- Manage customers and their tokens
- See the number of queries executed by customers, along with which features were used

This application provides an websocket API that is used by the `air` applications installed
on customer premises.


## What is it made up of

The `central` is written as a single [elixir](elixir-lang.org/) [phoenix](www.phoenixframework.org) website.

Additionally to run it relies on `Postgres` for datastorage.


## Getting started

### Prerequisites

In order to run the system you need the following components:

- Node.js 5 or newer
- Erlang and Elixir (see [here](../README.md#prerequisites) for details)
- Docker 1.11 (+ [Docker for Mac](https://docs.docker.com/docker-for-mac/) if on macOS)

Once you have all the main components, you also need the elixir and node.js dependencies required by our
application. Node.js is included to compile our javascript and css dependencies.

- `mix deps.get` installs our elixir and erlang dependencies
- `npm install` installs our node dependencies

Before you run the application for the first time, you also need to make sure you initialize the database
with `make recreate-db`
(make sure that [required common components are started](../README.md#starting-the-required-components)).


### Running

First, make sure the dependencies are started by running `../start_dependencies.sh`.
To start the development server, you run: `make start`.
Now you can access the site at http://localhost:7080/.

Note that there's no need to migrate the database. This will happen automatically when the application starts.
However, if you do need to manually migrate/rollback (for example while creating a new migration), you can do
it with `make migrate` and `make rollback` respectively.

If you need to repopulate the database, you can run `make recreate-db`. Keep in mind that this will erase all
of your existing data, so use with caution. To recreate the test database, you can run `MIX_ENV=test make recreate-db`

Notice that the statistics page of Central is not going to work locally. It relies on an nginx instance for
proxying the requests to within the MPI network. We could have setup a separate [Shiny
server](https://www.rstudio.com/products/shiny/shiny-server/) locally for development purposes, but this seems so low
priority to the point of being pointless.

#### HTTPS endpoint

The site also accepts HTTPS requests on port 7443. Self-signed certificates are provided for the site central.air-local, meaning you need to add `/etc/hosts` entry which points this hostname to 127.0.0.1. You'll also need to import the certificate (located in `./priv/config/ssl_cert.pem`) into your browser. Once all is setup, you can access the site over HTTPS at https://central.air-local:7443.

### Local docker container

To start a local docker container, you need to first build the image with `./build-image.sh`. Then you can start the container with `./container.sh console`. Once the container is started you can access it at http://localhost:7080/ and https://insights.air-local:7443. Since different ports are used, the container can run side-by-side to the local site.

__Linux developers__: Scripts in this project use docker in the context of the logged in user (without root
privileges). To enable this, you need to add yourself to the `docker` group. See
[here](http://askubuntu.com/a/477554) for explanation.

__macOS users__: see [here](../macos_docker.md) for additional instructions.

### Other common tasks

- tests: `make test`
- `mix coveralls.html` - runs ExUnit tests with test coverage (generates an HTML output in the `cover` folder)
- dialyzer: `make dialyze`
- documentation: `make docs`
- lint: `make lint`
- building a release: `make release` (`make release-local` on dev machine)

### Deploying

To deploy `central`, run the command `./production.sh <target> deploy` from the `central` root folder.

There are two deployment targets available. `central_prod` and `central_stage`. These deploy to
[https://central.aircloak.com](https://central.aircloak.com) and
[https://central-stage.aircloak.com](https://central-stage.aircloak.com) respectively.

If you want to change aspects of the deployment you can find the deployment configurations in the
[deploy_targets](../deploy_targets) folder alongside the configurations for the `aircloak` deployments.

Please note that all deployed aircloaks talk to the production central by default. That is by design.
To exercise the stage central, you have to visit [https://air-for-central-stage.aircloak.com](https://air-for-central-stage.aircloak.com)
which can be deployed like any other `aircloak` and relies on the deployment target `for_central_stage`.

#### One time proxy setup of nginx

Nginx is used to proxy to internal MPI services that should not be exposed directly to the internet. For these services
we rely on nginx managing the forwarding, and it calling back to the central app for authenticating the user. This
requires a little extra setup on the nginx server side:

- You need to define a single internal proxy authentication location nginx can use for authentication
- You need to define a forwarding location for each service you want forwarded

The configuration could look like the one below:

```
# Endpoint for resource that should be proxied
upstream stats_proxy {
  server srv-76-133.mpi-sws.org;
  keepalive 2;
}

upstream central {
  ...
}

server {
  ...

  location /stats_proxy/ {
    auth_request     /internal_proxy_auth;
    auth_request_set $auth_status $upstream_status;

    # needed for websockets
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";

    # we don't want nginx trying to do something clever with
    # redirects, we set the Host: header above already.
    proxy_redirect off;

    proxy_set_header Host $http_host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Host $host;
    proxy_set_header X-Forwarded-Server $host;
    # proxy_set_header X-Forwarded-Proto https;
    proxy_set_header X-Forwarded-Port $server_port;

    proxy_pass http://stats_proxy/stats/;
  }

  location = /internal_proxy_auth {
    internal;
    proxy_pass              http://central/proxy_auth;
    proxy_pass_request_body off;
    proxy_set_header        Content-Length "";
    proxy_set_header        X-Original-URI $request_uri;
  }

  ...
}
```
