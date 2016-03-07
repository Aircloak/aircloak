Air frontend
============

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Running](#running)
    - [Testing](#testing)
- [Licenses](#licenses)

----------------------

# What it does

This component implements HTTPS endpoints for the Air system. In addition it is responsible for database migrations.

# Getting started

## Running

You need to have `ruby` 2.0.0 and `bundler` installed. `etcd` and `db` containers must be running. Then, you can install gems, migrate the database, and start the server:

```
bundle install
bundle exec rake db:migrate
bundle exec rails s
```

The server is listening at the port 8080

## Testing

To run the test suite, make sure the test database is migrated:

    RAILS_ENV=test bundle exec rake db:create
    RAILS_ENV=test bundle exec rake db:migrate

then to run the tests use

    bundle exec rake

### Tests running against Github

Some of the tests run against Github. These interactions have been cached by the VCR gem. The requests were authenticated with an OAuth token that is no longer valid.
If you need to make a change to an existing test which invalidates a pre-recorded VCR, or you need to create new tests that communicate with Github, then you will need to perform the following steps:

- generate a valid OAuth token on Github on the __settings__ > __applications__ page
- set the OAuth token in the testing section of your `config/settings.yml` file
- update the OAuth token in the existing VCRs to match your new OAuth token
- record new VCRs as you desire
- invalidate the OAuth token on Github so we don't check a valid OAuth token into the source code repository

### Test with specific random seed

The order in which tests are run is randomized by rpsec. This helps uncover ordering dependencies between tests which sometimes camouflage real issues.
When you have found a order that breaks your tests, you can lock it in by setting the random seed used in `spec/spec_helper.rb`.

# Licenses

To get a list of the licenses in use by our project, we use the `gem-licenses` gem and the `rake gem:licenses`
rake task it provides. Check out the [documentation](http://www.rubydoc.info/gems/gem-licenses/0.2.0) for more
information. You can also have the licenses written to a file with `rake gem:licenses:csv['licenses.csv']`.
