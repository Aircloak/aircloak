require "bundler/capistrano"

load "config/recipes/base"
load "config/recipes/nginx"
load "config/recipes/postgresql"
load "config/recipes/unicorn"
load "config/recipes/check"

# We need to fudge the path a little to ensure capistrano
# finds the bundler gem and ruby while deploying.
set :default_environment, {
  'PATH' => "/home/deployer/.rbenv/shims:/home/deployer/.rbenv/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games",

  # When gems should be downloaded, we need to enable an http proxy.
  # For normal deployments, where the list of gems haven't changed,
  # nothing needs to be done.
  # When there are new or changed gems introduced, one needs to
  # enable the proxy for 12 hours at tintenfisch.mpi-klsb.mpg.de.
  # Please note that you can only get access to this proxy if you
  # are on the MPI network (meaning you either need to be physically
  # there, or using the VPN).
  'http_proxy' => "http://dmz-gw.mpi-klsb.mpg.de:3128/"
}


# This requires something along the lines of this
# in your ~/.ssh/config
#
#   Host air1
#     User deployer
#     ProxyCommand ssh [USERNAME]@contact.mpi-sws.org nc %h %p 2> /dev/null
#
server "air1", :web, :app, :db, primary: true

set :application, "aircloak"

# You need to run the capistrano setup and install tasks as root.
# Please adjust the user line for the very first run.
set :user, "deployer"

set :deploy_to, "/websites/#{application}"
set :deploy_via, :remote_cache

# We want to do all actions as the deployment user
set :use_sudo, false

set :scm, :git
set :repository,  "git@github.com:Aircloak/web.git"
set :branch, "master"
set :git_enable_submodules, 1

default_run_options[:pty]
ssh_options[:forward_agent] = true

# Cleans up old deploys, leaving only 5
after "deploy:restart", "deploy:cleanup"
