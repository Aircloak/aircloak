Air
===

The `air` is the entity through which our users interact with their cloaks.
It contains the interfaces for adding and administering users to those
needed to create, update and run tasks.

----------------------

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
- [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Running](#running)
    - [Testing](#testing)

----------------------

# What it does

This repository contains the Air system, which provides HTTPS endpoints that allows end-users, such as analysts and cluster maintainers, to perform various tasks, such as:

- Manage cloaks and data sources
- Manage users and their permissions
- Write, test, and execute queries

This application will also provide API endpoints that can be used by analysts to programmatically interact with
the system, as well as the end-points used by the cloaks when they communicate with the `air`.


# What is it made up of

The air is written as a single [elixir](elixir-lang.org/) [phoenix](www.phoenixframework.org) website. It
contains both the front end facing as well as the backend services needed to operate the system.

Additionally to run it relies on `etcd` for configuration and `Postgres` for datastorage.


# Getting started

## Prerequisites

In order to run the system you need the following components:

- Node.js 5 or newer
- Elixir 1.2 or newer
- Erlang 18

Once you have all the main components, you also need the elixir and node.js dependencies required by our
application. Node.js is included to compile our javascript and css dependencies.

- `mix deps.get` installs our elixir and erlang dependencies
- `npm install` installs our node dependencies

Before you run the application for the first time, you also need to make sure you migrate the database
with `make migrate`.


## Running

To start the development server, you run: `make start` which makes the website available on
`http://localhost:4000`. Additionally it starts the website in the interactive `iex` console, which allows
you to interact with the running application.


## Testing

You can run tests with `make test`. Dialyzer can be started with `make dialyzer` (you need at least Elixir 1.2.3 for it to work).
