require "bundler/capistrano"

load "config/recipes/base"
load "config/recipes/nginx"
load "config/recipes/passenger"
load "config/recipes/postgresql"
load "config/recipes/rbenv"
load "config/recipes/unicorn"
load "config/recipes/check"


# This requires something along the lines of this
# in your ~/.ssh/config
#
#   Host graphite
#     User deployer
#     ProxyCommand ssh [USERNAME]@contact.mpi-sws.org nc %h %p 2> /dev/null
#
server "graphite", :web, :app, :db, primary: true

set :application, "aircloak"
set :user, "deployer"
set :deploy_to, "/websites/#{application}"
set :deploy_via, :remote_cache
set :use_sudo, false

set :scm, :git
set :repository,  "git@github.com:Aircloak/web.git"
set :branch, "master"
set :git_enable_submodules, 1

default_run_options[:pty]
ssh_options[:forward_agent] = true

# Cleans up old deploys, leaving only 5
after "deploy:restart", "deploy:cleanup"
