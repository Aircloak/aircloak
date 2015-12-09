FROM ruby:2.0.0-p643
MAINTAINER Aircloak

AIR_INIT

# throw errors if Gemfile has been modified since Gemfile.lock
RUN bundle config --global frozen 1

RUN apt-get update && apt-get install nginx-light vim nano curl jq -y && apt-get -y upgrade

# According to many advices, we'll use gosu instead of sudo to step-down from root
# Apparently, there are some issues with sudo inside the docker container.
# For example, Docker docs (https://docs.docker.com/articles/dockerfile_best-practices/)
# state following:
# You should avoid installing or using sudo since it has unpredictable TTY and
# signal-forwarding behavior that can cause more problems than it solves. If you
# absolutely need functionality similar to sudo (e.g., initializing the daemon as
# root but running it as non-root), you may be able to use gosu.
RUN . /tmp/build_config/proxies.sh && \
  curl -o /usr/local/bin/gosu -sSL "https://github.com/tianon/gosu/releases/download/1.4/gosu-$(dpkg --print-architecture)" && \
  chmod +x /usr/local/bin/gosu


## ------------------------------------------------------------------
## Setup scaffolding
## ------------------------------------------------------------------

ENV site_path /aircloak/website
# Force production environment
ENV RAILS_ENV production

RUN mkdir -p $site_path/tmp
RUN mkdir -p $site_path/log

# We log to a volume, so the logs don't fill up
# the container
VOLUME $site_path/log
VOLUME /var/log

# User under which the app will run.
RUN useradd --no-create-home --shell /bin/bash deployer
RUN chown -R deployer:deployer /usr/local/bundle


## ------------------------------------------------------------------
## Compile APIdocs
## ------------------------------------------------------------------

# We perform gem downloads in a separate step to make
# it cacheable and independent of the values of the web
# application itself.
#
# We first work on the slate gems (which are unlikely to change)
# and then the web Gem's which are more likely to change.

# -----> slate gems
RUN mkdir $site_path/slate
COPY frontend/slate/Gemfile frontend/slate/Gemfile.lock $site_path/slate/
WORKDIR $site_path/slate

# Setup bundle_with_retry script which retries in case of failures, such as network timeouts
RUN echo 'bundle "$@" || bundle "$@" || bundle "$@" || bundle "$@" || bundle "$@"' \
  > /tmp/bundle_with_retry && chmod +x /tmp/bundle_with_retry
RUN . /tmp/build_config/proxies.sh && /tmp/bundle_with_retry install --path vendor/bundle && \
  rm -rf /aircloak/website/vendor/bundle/ruby/2.0.0/cache

# -----> rails gems
WORKDIR $site_path
COPY frontend/Gemfile frontend/Gemfile.lock $site_path/
RUN . /tmp/build_config/proxies.sh && /tmp/bundle_with_retry install \
    --without development:test \
    --path vendor/bundle \
    --binstubs vendor/bundle/bin \
    -j4 \
    --deployment \
    --no-clean && \
    rm -rf /aircloak/website/vendor/bundle/ruby/2.0.0/cache


## ------------------------------------------------------------------
## Create API docs in a cacheable manner
## ------------------------------------------------------------------

WORKDIR $site_path/slate
COPY frontend/slate $site_path/slate
RUN . /tmp/build_config/proxies.sh && bundle exec middleman build


## ------------------------------------------------------------------
## Setup nginx
## ------------------------------------------------------------------

COPY frontend/config/nginx.conf /tmp/nginx.conf

# Nginx logs at /var/log/nginx
# We want to tail that, so make it accessible to us
RUN mkdir -p /var/log/nginx
RUN chown -R deployer:deployer /var/log/nginx

# This is needed by nginx
RUN chown -R deployer:deployer /var/lib/nginx


## ------------------------------------------------------------------
## Setup rails app
## ------------------------------------------------------------------

WORKDIR $site_path

# We selectively copy across the things we want.
# This prevents caching mishaps that aren't needed,
# and would result in having to spend time on
# docker builds (which, let me tell you(!!!) are
# a waste of your previous time!).
COPY frontend/app $site_path/app
COPY frontend/bin $site_path/bin
COPY frontend/config $site_path/config
COPY frontend/db $site_path/db
COPY frontend/lib $site_path/lib
COPY frontend/public $site_path/public
COPY frontend/vendor $site_path/vendor
COPY frontend/config.ru $site_path/config.ru
COPY frontend/Rakefile $site_path/Rakefile

# Create static assets that can be cached and served
# Note: Rails 4 always initializes the database, even when precompiling assets. To avoid this, we hack a bit:
#   - We use a distinct precompile env which is for all intents and purposes the same as production
#   - In aircloak_config.rb we don't attempt to read from ETCD store if in precompile env
RUN cp -rp config/environments/production.rb config/environments/precompile.rb
RUN RAILS_ENV=precompile bundle exec rake assets:precompile
RUN rm -rf tmp/cache

# At this point we can copy in the apidocs we previously compiled.
RUN mv $site_path/slate/build/ $site_path/public/apidocs
RUN rm -rf $site_path/slate

# We also want the docker folder which contains script
# we use when running the app. We copy this as a late step
# as it isn't needed in any of the compilation steps,
# and changes to it therefore shouldn't invalidate the cache
COPY frontend/docker $site_path/docker
COPY config/config.sh $site_path/docker/config.sh
COPY config/tcp_ports.json $site_path/docker/tcp_ports.json

# Needed so nginx and unicorn can access the site
RUN chown -R deployer:deployer $site_path

# We'll run as root, but step-down in the init script to the non-privileged user
USER root

AIR_TAG_VERSION
