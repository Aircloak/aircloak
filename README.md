Aircloak Web
============

[![Code
Climate](https://codeclimate.com/repos/52834e57c7f3a36f49049694/badges/b93de74f2a883ef1f819/gpa.png)](https://codeclimate.com/repos/52834e57c7f3a36f49049694/feed)
[![Build
Status](https://magnum.travis-ci.com/Aircloak/web.png?token=aFqD8qTNFV1Li4zdKtZw&branch=master)](https://magnum.travis-ci.com/Aircloak/web)

The web-frontend of Aircloak, and the RESTful API for storing and retrieving
queries and results.

# Setup
Make sure you hande a recent version of ruby. We use Ruby 2.0 for development and deployment.
I recommend installing your ruby using the [Ruby Version Manager](https://rvm.io).

Next, please install the `bundler gem`:

    gem install bundler

To get the dependencies of the aircloak web app, run the following
command in the root of the repository:

    bundle install

# PRO tip

Rails can be dog-slow, or slow like a glacier if you prefer. To speed things up, I recommend installing the
zeus gem. You can do this with `gem install zeus`. Zeus preloads rails for you. When you subsequently want to
perform an action that requires rails to be loaded, it forks the preloaded copy of rails (which happens
significantly faster than spawning the whole process anew) and performs the operation.

A normal workflow looks like this:

    zeus start # starts the zeus daemon, you need to leave this running in a tab somewhere
    zeus s # starts the rails server
    zeus rake SOMETHING # some rake task that otherwise would have been slow
    zeus g|d controller|model|migration # use the rails generators in no time
    zeus rspec spec/path/to/test # this is good for tests that require rails.
    spec/test spec/lib/some_test_spec.rb # this is still significantly faster if your test does not include
    rails

# Running the app

You need to migrate the database:

    bundle exec rake db:create
    bundle exec rake db:migrate

    ...

    rails s

For easy development, consider using [pow](http://pow.cx).

# Deploying to production

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

## Migrating the schema 

To migrate the schema on the production database, please run:

    bundle exec cap deploy:migrate

## Deploy website

To deploy the website, run

    bundle exec cap deploy

Please note that doing a `cap deploy`, while restarting the unicorn workers, does not always have them reload
the classes in the __lib__ directory. I haven't investigated why this is. If it happens to you, stop and start
the unicorns like this:

    bundle exec cap unicorn:stop
    bundle exec cap unicorn:start

Happy deploying!
