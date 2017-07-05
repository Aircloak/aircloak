# Documentation

- [What it is](#what-it-is)
- [What is it made up of](#what-is-it-made-up-of)
- [Getting started](#getting-started)
    - [Dependencies](#dependencies)
    - [Testing](#testing)
- [Plugins](#plugins)

----------------------

## What it is

This folder contains the user documentation for the Aircloak system.
The documentation spans everything from the query language, to how
the system can be configured and peculiarities of different backends.

## What is it made up of

The documentation is written with `gitbook`. For more information about
gitbook, check out the [toolchain](https://toolchain.gitbook.com)
guide. It describes how the [content/README.md](content/README.md),
[content/GLOSSARY.md](content/GLOSSARY.md) etc work.

## Getting started

### Dependencies

Dependencies are managed with `yarn`. To get up and running, run `yarn install`.

If you also want to create PDF, epub and mobi versions of the docs, you need to have `ebook-convert` installed. Follow
the [gitbook ebook instructions](https://toolchain.gitbook.com/ebook.html) for
more details.

### Testing

While (or after) you have written your text, it makes sense to see how it would
look in book form. A development server can be started with:

```
yarn run gitbook serve
```

which makes a version of the docs available under [localhost:4000](http://localhost:4000).

To produce a final book, you can run `yarn run gitbook build`.

## Plugins

Gitbook uses a series of plugins for different functionality.
These plugins are defined in [content/book.js](content/book.js).
The plugins which are delivered as npm packages, should be managed
by `yarn`, so make sure to also add them there.

A plugin called `foo` will usually have the package name `gitbook-plugin-foo`.
