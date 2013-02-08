Aircloak Web
============

The web-frontend of Aircloak, and the RESTful API for storing and retrieving
queries and results.

# Setup
Make sure you hande a recent version of ruby. Rails 4 required 1.9.3 or newer.
I recommend installing your ruby using the [Ruby Version Manager](https://rvm.io).

Next, please install the `bundler gem`:

    gem install bundler

To get the dependencies of the aircloak web app, please run the following
command in the root of the repository:

    bundle install

# Running the app

You need to migrate the database:

    bundle exec rake db:create
    bundle exec rake db:migrate

    ...

    rails s

For easy development, consider using [pow](http://pow.cx).

-------------------------------------------
# The things below are from the old readme.
# They do not yet apply to this version of the app
-------------------------------------------

# Running tests while developing

Make sure you have setup your database

    bundle exec rake db:test:load

Run autotest

    bundle exec autotest

It will rerun tests when files change.
If you want to run a single test in isolation, run

    bundle exec rake test -Itest test/path/to/testfile

# Have mailers and background tasks work in development

To get the mailer to work, you need to set the correct mandrill 
username and password file in your environment.

Assuming you are using RVM for Ruby, please adapt the sample rvmrc file:

    cp rvmrc .rvmrc

Update it to contain the correct mandrill username and password.
To start or stop the delayed job, run either of the following:

    ./start_delayed_job.sh
    ./stop_delayed_job.sh
