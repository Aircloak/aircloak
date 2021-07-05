# Air

The `air` is the entity through which our users interact with their cloaks.
It contains the interfaces for adding and administering users to those
needed to create, update and run tasks.

---

- [Air](#air)
  - [What it does](#what-it-does)
  - [What is it made up of](#what-is-it-made-up-of)
  - [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Running](#running)
      - [HTTPS endpoint](#https-endpoint)
      - [LDAP server](#ldap-server)
    - [Local docker container](#local-docker-container)
    - [Other common tasks](#other-common-tasks)
    - [Deploying](#deploying)
  - [Frontend](#frontend)
    - [Analyst pages](#analyst-pages)
    - [Admin pages](#admin-pages)

---

## What it does

This repository contains the Air system, which provides HTTPS endpoints that allows end-users, such as analysts and cluster maintainers, to perform various tasks, such as:

- Manage cloaks and data sources
- Manage users and their permissions
- Write, test, and execute queries

This application also provides API endpoints that can be used by analysts to programmatically interact with
the system, as well as the end-points used by the cloaks when they communicate with the `air`.

## What is it made up of

The air is written as a single [elixir](elixir-lang.org/) [phoenix](www.phoenixframework.org) website. It
contains both the front end facing as well as the backend services needed to operate the system.

Additionally to run it relies on `Postgres` for datastorage.

## Getting started

### Prerequisites

In order to run the system you need Erlang, Elixir, NodeJS and yarn. If you followed the
[prerequisites](../README.md#prerequisites) section in the main README, you should
already have the right version of the three first installed and managed through asdf.
Yarn you can get from the [yarnpkg](https://yarnpkg.com/).

Once the language environment is setup, run the following two commands to download the dependencies:

- `mix deps.get` installs our elixir and erlang dependencies
- `cd assets && yarn install` installs our node dependencies

Before you run the application for the first time, you also need to make sure you initialize the database
with `make recreate-db`
(make sure that [required common components are started](../README.md#starting-the-required-components)).

The cloak is configured to connect to `air` on `insights.air-local`. To make this work you will need
to edit your `/etc/hosts` file to point `insights.air-local` to `127.0.0.1` by adding the line.

### Running

First, make sure the dependencies are started by running `../start_dependencies.sh`. To start the development server, you run: `make start`. Now you can access the
site at http://localhost:8080/. In development there is a default admin user with username: `admin@aircloak.com` and password: `psswrd12`.

Note that there's no need to migrate the database. This will happen automatically when the application starts.
However, if you do need to manually migrate/rollback (for example while creating a new migration), you can do
it with `make migrate` and `make rollback` respectively.

If you need to repopulate the database, you can run `make recreate-db`. Keep in mind that this will erase all
of your existing data, so use with caution. To recreate the test database, you can run `MIX_ENV=test make recreate-db`

#### HTTPS endpoint

The site also accepts HTTPS requests on port 8443. Self-signed certificates are provided for the site insights.air-local, meaning you need to add `/etc/hosts` entry which points this hostname to 127.0.0.1. You'll also need to import the certificate (located in `./priv/config/ssl_cert.pem`) into your browser. Once all is setup, you can access the site over HTTPS at https://insights.air-local:8443.

#### LDAP server

One of the dependencies is an LDAP server. In fact two instances are started - one for test, listening on ports 389 and
636, and another for dev, listening on ports 1389 and 1636. Do not modify the contents of the former one. The latter
can be modified freely.

Both servers are seeded with the contents of the file `ldap/bootstrap.ldif`, creating two users `alice` and `bob` with
passwords `psswrd12`. The dev environment is configured to sync with the dev server, so assuming the server is
running it should be possible to login with those credentials. The LDAP administrator credentials are
`cn=admin,dc=example,dc=com` with password `admin`.

### Local docker container

To start a local docker container, you need to first build the image with `./build-image.sh`. Then you can start the container with `./container.sh console`. Once the container is started you can access it at http://localhost:8080/ and https://insights.air-local:8443. Since different ports are used, the container can run side-by-side to the local site.

**Linux developers**: Scripts in this project use docker in the context of the logged in user (without root
privileges). To enable this, you need to add yourself to the `docker` group. See
[here](http://askubuntu.com/a/477554) for explanation.

**macOS users**: see [here](../macos_docker.md) for additional instructions.

### Other common tasks

- tests: `make test`
- `mix coveralls.html` - runs ExUnit tests with test coverage (generates an HTML output in the `cover` folder)
- dialyzer: `make dialyze`
- documentation: `make docs`
- lint: `make lint`
- building a release: `make release` (`make release-local` on dev machine)

### Deploying

See [here](../README.md#deploying).

## Frontend

### Analyst pages

This includes any and all pages that are visible to a user (logged in or not) who does not have administrative
privileges. These pages should be optimized for screens 1024px and wider. That means that on those screens all
elements should be visible (not hidden or collapsed) and all text items appropriately truncated to fit into the
layout or broken up into multiple lines.

On screens 360px and wider these pages should be at least functional. This means that all basic tasks can be
accomplished, however some non-key elements can be collapsed or hidden entirely. The basic tasks are:

- logging in
- changing your password (in case you need to do that on the go due to a breach)
- selecting a data source
- write and run a query
- see the results of a query

### Admin pages

This includes all pages that require administrative privileges to view. These should be optimized for 1024px like
analyst pages. No extra effort should go into making them work on smaller screens beyond what's provided by
bootstrap.
