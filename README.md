Aircloak Web
============

[![Code
Climate](https://codeclimate.com/repos/52834e57c7f3a36f49049694/badges/b93de74f2a883ef1f819/gpa.png)](https://codeclimate.com/repos/52834e57c7f3a36f49049694/feed)
[![Build
Status](https://magnum.travis-ci.com/Aircloak/web.png?token=aFqD8qTNFV1Li4zdKtZw&branch=master)](https://magnum.travis-ci.com/Aircloak/web)

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [API](#api)
    - [Running](#running)
    - [Deploying](#deploying)
    - [Setting up new servers](#setting-up-new-servers)
    - [Testing](#testing)
    - [Good to know](#good-to-know)
- [Role in the greater picture](#role-in-the-greater-picture)
- [What it is made up of](#what-it-is-made-up-of)
    - [Components](#components)

----------------------

# What it does

This repository contains the web-frontend for the aircloak backend control system, as well as the RESTful API for programatically interacting with our infrastructure.

# Getting started
## API

The web currently provides an API namespaced under `/api/
The following resources are exposed under the API:

- machines
- clusters
- queries
- version tests

This API is under flux. The best resource is the code. To get an overview, please have a look at the
[routes](https://github.com/Aircloak/web/blob/master/config/routes.rb#L57) file.


## Running

We use Ruby 2.0 for development and deployment.
I recommend installing your ruby using the [Ruby Version Manager](https://rvm.io).

Next, please install the `bundler gem`:

    gem install bundler

To get the dependencies of the aircloak web app, run the following
command in the root of the repository:

    bundle install

Before being able to run the application, you need to create and migrate the database:

    bundle exec rake db:create
    bundle exec rake db:migrate

You can run the application with `rails s`, but I recommend using zeus. Please see the [good to
know](#goot-to-know) section below.


## Deploying

We use [Capistrano](https://github.com/capistrano/capistrano) for deployment. You already get it installed
automatically when running `bundle install`.
We could have used any number of other fancy approaches for deploying the website, but capistrano has been
setup, and it works well, so I suggest we leave it like this until we find a compelling reason to switch to
a more superior system.

In order to deploy the website, you need to be able to log into the web servers as the `deployer` user.
I recommend you add the following to your ~/.ssh/config:

    Host graphite
      User deployer
      ProxyCommand ssh [YOUR MPI USERNAME]@contact.mpi-sws.org nc %h %p 2> /dev/null

If you are not able to log into the web servers, please ask someone who is able to do so, to add your public
key to the __deployer__ user's __authorized_keys__ file.

### Migrating the schema 

To migrate the schema on the production database, please run:

    bundle exec cap deploy:migrate

### Deploy website

To deploy the website, run

    bundle exec cap deploy

Please note that doing a `cap deploy`, while restarting the unicorn workers, does not always have them reload
the classes in the __lib__ directory. I haven't investigated why this is. If it happens to you, stop and start
the unicorns like this:

    bundle exec cap unicorn:stop
    bundle exec cap unicorn:start

Happy deploying!


## Setting up new servers

Setting up new web servers unfortunately is still a bit of a manual process.
Currently there are a set of commands that need to be run manually, and then
the rest can be done automatically.

### Step 1: Manual install

Please log onto the machine as root, and do the following steps:

```bash
apt-get update && apt-get -y upgrade
apt-get install -y autoconf nginx curl git-core

# Dependencies needed for ruby
apt-get -y install build-essential libssl-dev
# Packages required for compilation of some stdlib modules
apt-get -y install tklib 
# Extras for RubyGems and Rails:
apt-get -y install zlib1g-dev libssl-dev
# Readline Dev on Ubuntu 12.04 LTS:
apt-get -y install libreadline-gplv2-dev
# Install some nokogiri dependencies:
apt-get -y install libxml2 libxml2-dev libxslt1-dev
# Needed for pg gem
apt-get -y install libpq-dev 

# Add deployment user
adduser --disabled-password --gecos "" deployer
mkdir /websites
chown -R deployer:deployer /websites

touch /etc/init.d/unicorn_aircloak
chown deployer:deployer /etc/init.d/unicorn_aircloak
chmod +x /etc/init.d/unicorn_aircloak
update-rc.d -f unicorn_aircloak defaults

su - deployer
curl -L https://raw.github.com/fesplugas/rbenv-installer/master/bin/rbenv-installer | bash

touch ~/.bashrc
echo "export RBENV_ROOT=\"\${HOME}/.rbenv\"
if [ -d \"\${RBENV_ROOT}\" ]; then
  export PATH=\"\${RBENV_ROOT}/bin:\${PATH}\"
  eval \"\$(rbenv init -)\"
fi" >> ~/.bashrc
source ~/.bashrc

rbenv install 2.0.0-p247
rbenv global 2.0.0-p247

gem install bundler --no-ri --no-rdoc
rbenv rehash

```

### Step 2: Configure nginx

Please manually copy
[config/nginx.conf](https://github.com/Aircloak/web/blob/master/config/nginx.conf) to __/etc/nginx/nginx.conf__ and then run `/etc/init.d/nginx
restart`.

### Step 3: Configure ~/.ssh/config

From now on you want to log onto the machine as the __deployer__ user when you push changes.
Please adapt your __~/.ssh/config__ file such that when logging into the host, you are the __deployer__ user.

Example ~/.ssh/config


    ## ------------------------------------------------------------------
    ## Web hosts
    ## ------------------------------------------------------------------

    Host air1-root
      User root
      HostName air1
      ProxyCommand ssh [USER-NAME]@contact.mpi-sws.org nc %h %p 2> /dev/null

    Host air1
      User deployer
      ProxyCommand ssh [USER-NAME]@contact.mpi-sws.org nc %h %p 2> /dev/null

You also need to have you public key added to the __~/.ssh/authorized_keys__ file for the deployer user for
this to work.

### Step 4: Setup and deploy

Now run `bundle exec cap deploy:setup` to setup the infrastructure.
For the initial deployment, the application is also going to want to download all the ruby gems. The server
itself is sitting in a restricted network, without access to outside resources. Therefore you need to enable
an http proxy for the server for the duration of the install. For instructions, please follow this
[guide](https://github.com/Aircloak/org/wiki/admin::Useful-tips-and-tricks#wiki-getting-web-access-from-a-system-within-the-dmz).
Once the proxy has been setup, run `bundle exec cap deploy:cold`, and you should be good to go!


## Testing

To run the test suite, make sure you have the test database created:

    RAILS_ENV=test bundle exec rake db:create
    RAILS_ENV=test bundle exec rake db:migrate

then to run the tests use

    bundle exec rake

## Good to know

Rails can be dog-slow, or slow like a glacier if you prefer. To speed things up, I recommend installing the
zeus gem. You can do this with `gem install zeus`. Zeus preloads rails for you. When you subsequently want to
perform an action that requires rails to be loaded, it forks the preloaded copy of rails (which happens
significantly faster than spawning the whole process anew) and performs the operation.

A normal workflow looks like this:

```
zeus start # starts the zeus daemon, you need to leave this running in a tab somewhere
zeus s # starts the rails server
zeus rake SOMETHING # some rake task that otherwise would have been slow
zeus g|d controller|model|migration # use the rails generators in no time
zeus rspec spec/path/to/test # this is good for tests that require rails.
spec/test spec/lib/some_test_spec.rb # this is still significantly faster if your test does not include
rails
```

# Role in the greater picture

The web component takes on a lot of different roles.
It allows us to manage our clusters. We can add physical nodes and create cluster, and see the health of
clusters and individual cloaks. It also allows us to see the output of building individual deployable
entities.

The web also currently has the legacy role of providing integration with our windows testing machines for the
windows testing client, and providing and endpoint at which windows clients can connect and ask for newer
versions of the windows client. These components currently exist, but aren't actively in use.

The web will in the future also provide means for analysts to upload queries and for them to see the result
of running these queries.

# What it is made up of
## Components

We have three main components of interest in our system:

* Deployable entities
* Clusters and cloaks
* Tasks

### Deployable entities

A [deployable entity](https://github.com/Aircloak/web/blob/master/app/models/deployable_entity.rb) is a program of some sort that is developed by us and that is part of a cloak deployment.
Examples of such systems would be [erlattest](https://github.com/Aircloak/erlattest), and
[cloak-core](https://github.com/Aircloak/cloak-core).

Deployable entities come in multiple versions. These are called [deployable entity
versions](https://github.com/Aircloak/web/blob/master/app/models/deployable_entity_version.rb) where each
version corresponds to a commit in git.

A group of built and packaged deployable entities entity versions (one for each deployable entity) make up a [build](https://github.com/Aircloak/web/blob/master/app/models/build.rb). A build in turn can be installed onto a machine and makes it into a cloaked machine.

### Clusters and cloaks

A [cluster](https://github.com/Aircloak/web/blob/master/app/models/cluster.rb) represents a set of machines
known as [cloaks](https://github.com/Aircloak/web/blob/master/app/models/cloak.rb). It ties these together in a logical
unit that all communicate together and run the same version of our system.

A cluster knows which specific version of our adapted debian system should be used on cloaks that are part
of the cluster, and also know which build (group of deployable entity versions) should be used.

When a cloak is added to an existing cluster, [manny-air](https://github.com/Aircloak/manny-air) is notified
and ensures the right software is installed on the cloak.

### Tasks

[Tasks](https://github.com/Aircloak/web/blob/master/app/models/task.rb) are what are executed on cloak clusters.
Currently they exist as a collection of Java class file binaries that collectively make up a Java application that
can make meaning of user data stored in the cloak.

Tasks are currently associated with clusters through
[queries](https://github.com/Aircloak/web/blob/master/app/models/query.rb), although the name _query_ is likely to
change in the future.

Queries in turn have indices, but these are currently in flux, and will therefore not be discussed further here.


### Version tests

Each new version of a deployable entity that passes automatic unit testing in our continuous integration
environment, automatically spawns an automated integration test. The process of spawning such a test
seems quite complex at first sight. It encompasses a lot of the web code base as well as three external systems.
This section will try to shed some light on what is going on behind the scenes.

When a new deployable entity version is created, this automatically also creates a new [version
test](https://github.com/Aircloak/web/blob/master/app/models/version_test.rb). 
The version test creates a new build that can be used for testing. This build includes the particular
deployable entity version under test, as well as the most recent version of the other deployable entities as
can be found in their respective "develop" branches.
Once the [build server](https://github.com/Aircloak/buildserver) has successfully created the build, a cluster is created that uses this particular
build, as well as the most recent version of the debian image as determined by its
[OsTag](https://github.com/Aircloak/web/blob/master/app/models/os_tag.rb).
Once the cluster is configured and running as reported by [manny-air](https://github.com/Aircloak/manny-air), the version test informs the [test
server](https://github.com/Aircloak/testserver) that is should run its test on the particular cluster.
The test server exercises the cluster and uses the API in the web system to verify that the cluster behaves
correctly.

If any of the steps above fail, the test is considered to have failed as well.
