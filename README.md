Aircloak
========

[![Build
Status](https://travis-ci.com/Aircloak/aircloak.svg?token=SwtqZyez24jMwX5xQx9U&branch=develop)](https://magnum.travis-ci.com/Aircloak/aircloak)

This repository contains the entirety of the Aircloak system.
A system that allows databases to be queried while enforcing
the anonymity of the individuals in the dataset.

There are two main components:

- __air__: a central control point only operating on non-sensitive data
- __cloak__: a component deployed close to the data, performing the querying and anonymization

## Prerequisites

You need to have Erlang and Elixir installed. The required versions are stated in [this file](.tool-versions). The easiest way to install is to use [asdf version manager](https://github.com/asdf-vm/asdf). You need to install it, together with Erlang and Elixir plugins. Then from the project root folder run `asdf install`. Before installing, make sure you have `unixodbc` installed (__OS X developers__ see [here](./cloak/osx_erlang_with_odbc.md) for detailed instructions).
