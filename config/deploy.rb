require "bundler/capistrano"
require "whenever/capistrano"

load "config/recipes/base"
load "config/recipes/nginx"
load "config/recipes/postgresql"
load "config/recipes/unicorn"
load "config/recipes/check"
load "config/recipes/settings"

# We need to fudge the path a little to ensure capistrano
# finds the bundler gem and ruby while deploying.
set :default_environment, {
  # When gems should be downloaded, we need to enable an http proxy.
  # For normal deployments, where the list of gems haven't changed,
  # nothing needs to be done.
  # When there are new or changed gems introduced, one needs to
  # enable the proxy for 12 hours at tintenfisch.mpi-klsb.mpg.de.
  # Please note that you can only get access to this proxy if you
  # are on the MPI network (meaning you either need to be physically
  # there, or using the VPN).
  # This flag cannot be set when deploying the web application
  # when there are no gem changes needed. The reason is that
  # the web application will attempt to do all inter 
  # application communication over the proxy, and fail.
  # An example is when communicating with the buildserver.
  # Please also note that when doing a deploy that requires
  # an update to the gems (i.e. the proxy is enabled),
  # then one needs to run a `bundle exec cap unicorn:stop`
  # followed by `bundle exec cap unicorn:start` afterwards,
  # to ensure the web application runs without the proxy
  # environment variable afterwards.
  # 'http_proxy' => "http://dmz-gw.mpi-klsb.mpg.de:3128/",

  'PATH' => "/home/deployer/.rbenv/shims:/home/deployer/.rbenv/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games"
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
set :branch, "develop"
set :git_enable_submodules, 1

default_run_options[:pty]
ssh_options[:forward_agent] = true

# Cleans up old deploys, leaving only 5
after "deploy:restart", "deploy:cleanup"

namespace :deploy do
  desc "Update the crontab file"
  task :update_crontab, :roles => :db do
    run "cd #{release_path} && whenever --update-crontab #{application}"
  end
end
after "deploy:create_symlink", "deploy:update_crontab"
