FROM ruby:2.0.0-p643
MAINTAINER Sebastian Probst Eide <sebastian@aircloak.com>

# throw errors if Gemfile has been modified since Gemfile.lock
RUN bundle config --global frozen 1

RUN apt-get update
RUN apt-get install nginx-light -y


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

# nginx will be listening on 8080
EXPOSE 8080

# User under which the app will run.
# Confd will also run as the same user
RUN useradd --no-create-home --shell /bin/bash deployer
RUN chown -R deployer:deployer /usr/local/bundle


## ------------------------------------------------------------------
## Get confd
## ------------------------------------------------------------------

WORKDIR /
RUN curl -O -L https://github.com/kelseyhightower/confd/releases/download/v0.9.0/confd-0.9.0-linux-amd64
RUN mv /confd-0.9.0-linux-amd64 /usr/local/bin/confd
RUN chmod +x /usr/local/bin/confd
RUN chown deployer:deployer /usr/local/bin/confd

# Setup confd so we can get the config for the cloak right
RUN mkdir -p /etc/confd/conf.d
RUN mkdir -p /etc/confd/templates
COPY config/confd/conf.d /etc/confd/conf.d/
COPY config/confd/templates /etc/confd/templates/
RUN chown -R deployer:deployer /etc/confd


## ------------------------------------------------------------------
## Setup nginx
## ------------------------------------------------------------------

COPY config/nginx.conf /etc/nginx/nginx.conf

# Nginx logs at /var/log/nginx
# We want to tail that, so make it accessible to us
RUN mkdir -p /var/log/nginx
RUN chown -R deployer:deployer /var/log/nginx

# This is needed by nginx
RUN chown -R deployer:deployer /var/lib/nginx


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
COPY slate/Gemfile $site_path/slate/
COPY slate/Gemfile.lock $site_path/slate/
WORKDIR $site_path/slate
RUN bundle install \
    --path vendor/bundle

# -----> rails gems
WORKDIR $site_path
COPY Gemfile $site_path/
COPY Gemfile.lock $site_path/
RUN bundle install \
    --without development:test \
    --path vendor/bundle \
    --binstubs vendor/bundle/bin \
    -j4 \
    --deployment \
    --no-clean


## ------------------------------------------------------------------
## Create API docs in a cacheable manner
## ------------------------------------------------------------------

WORKDIR $site_path/slate
COPY slate $site_path/slate
RUN bundle exec middleman build


## ------------------------------------------------------------------
## Setup rails app
## ------------------------------------------------------------------

WORKDIR $site_path

# We selectively copy across the things we want.
# This prevents caching mishaps that aren't needed,
# and would result in having to spend time on
# docker builds (which, let me tell you(!!!) are
# a waste of your previous time!).
COPY app $site_path/app
COPY bin $site_path/bin
COPY config $site_path/config
COPY db $site_path/db
COPY lib $site_path/lib
COPY public $site_path/public
COPY vendor $site_path/vendor
COPY config.ru $site_path/config.ru
COPY Rakefile $site_path/Rakefile

# Create static assets that can be cached and served
ENV ETCD_HOST 172.17.42.1
ENV ETCD_PORT 4001
RUN bundle exec rake assets:precompile
RUN rm -rf tmp/cache

# At this point we can copy in the apidocs we previously compiled.
RUN mv $site_path/slate/build/ $site_path/public/apidocs
RUN rm -rf $site_path/slate

# We also want the docker folder which contains script
# we use when running the app. We copy this as a late step
# as it isn't needed in any of the compilation steps,
# and changes to it therefore shouldn't invalidate the cache
COPY docker $site_path/docker

# Needed so nginx and unicorn can access the site
RUN chown -R deployer:deployer $site_path

USER deployer

CMD /aircloak/website/docker/start.sh
