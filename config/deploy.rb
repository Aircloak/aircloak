require "bundler/capistrano"

load "config/recipes/base"
load "config/recipes/nginx"
load "config/recipes/passenger"
load "config/recipes/postgresql"
load "config/recipes/rbenv"
load "config/recipes/check"

server "graphite", :web, :app, :db, primary: true

set :application, "aircloak"
set :user, "root"
set :deploy_to, "/websites/#{application}"
set :deploy_via, :remote_cache
set :use_sudo, false

set :scm, :git
set :repository,  "git@github.com:Aircloak/web.git"
set :branch, "master"
set :git_enable_submodules, 1

set :gateway, "spe@contact.mpi-sws.org"

default_run_options[:pty]
ssh_options[:forward_agent] = true

# Cleans up old deploys, leaving only 5
after "deploy:restart", "deploy:cleanup"
