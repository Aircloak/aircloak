# Air

The `air` is the entity through which our users interact with their cloaks.
It contains the interfaces for adding and administering users to those
needed to create, update and run tasks.

----------------------

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
- [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Running](#running)
    - [Other common tasks](#other-common-tasks)

----------------------

## What it does

This repository contains the Air system, which provides HTTPS endpoints that allows end-users, such as analysts and cluster maintainers, to perform various tasks, such as:

- Manage cloaks and data sources
- Manage users and their permissions
- Write, test, and execute queries

This application will also provide API endpoints that can be used by analysts to programmatically interact with
the system, as well as the end-points used by the cloaks when they communicate with the `air`.


## What is it made up of

The air is written as a single [elixir](elixir-lang.org/) [phoenix](www.phoenixframework.org) website. It
contains both the front end facing as well as the backend services needed to operate the system.

Additionally to run it relies on `Postgres` for datastorage.


## Getting started

### Prerequisites

In order to run the system you need the following components:

- Node.js 5 or newer
- Erlang and Elixir (see [here](../../README.md#prerequisites) for details)
- Ruby 2.x and bundler (for building API docs)

Once you have all the main components, you also need the elixir and node.js dependencies required by our
application. Node.js is included to compile our javascript and css dependencies.

- `mix deps.get` installs our elixir and erlang dependencies
- `npm install` installs our node dependencies

Before you run the application for the first time, you also need to make sure you initialize the database
with `make recreate_db`
(make sure that [required common components are started](../README.md#starting-the-required-components)).


### Running

To start the development server, you run: `make start`.
Assuming [common components are started](../README.md#starting-the-required-components), you can access the
site at https://insights.air-local:20000/.

Note that there's no need to migrate the database. This will happen automatically when the application starts.
However, if you do need to manually migrate/rollback (for example while creating a new migration), you can do
it with `make migrate` and `make rollback` respectively.

If you need to repopulate the database, you can run `make recreate_db`. Keep in mind that this will erase all
of your existing data, so use with caution. To recreate the test database, you can run `MIX_ENV=test make recreate_db`


### Other common tasks

- tests: `make test`
- `mix coveralls.html` - runs ExUnit tests with test coverage (generates an HTML output in the `cover` folder)
- dialyzer: `make dialyze`
- documentation: `make docs`
- lint: `make lint`
- building a release: `make release`
