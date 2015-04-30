# config valid only for Capistrano 3.2.1
lock '3.2.1'

set :application, 'air'
set :git_strategy, GitStrategy
set :repo_url, 'git@github.com:Aircloak/air.git'
set :branch, "develop"
set :deploy_to, '/aircloak/air'
set :linked_dirs, %w{log}
set :keep_releases, 3
# Prevents key hijacking.
set :ssh_options, {:forward_agent => false}

namespace :deploy do
  # We're building in the build subfolder. Then we're moving the built OTP release
  # to the parent (release) folder, and remove build sources.
  task :build do
    on roles(:app), in: :sequence, wait: 5 do
      execute "cd #{release_path}/build && make && make rel"
      execute "mv #{release_path}/build/rel/air/* #{release_path}"
      execute "rm -rf #{release_path}/build"
    end
  end

  task :stop do
    on roles(:app), in: :sequence, wait: 5 do
      # It's not an error if stop fails - perhaps the system was not running at all.
      execute "#{current_path}/bin/air stop || exit 0"
    end
  end

  task :start do
    on roles(:app), in: :sequence, wait: 5 do
      execute "#{current_path}/bin/air start"
    end
  end

  # Deploy workflow:
  #   1. fetch the code.
  #   2. Build the release.
  #   3. Stop the currently running version.
  #   4. Publish (change current symlink to point the new release)
  #   5. Start the new release.
  after :updated, :build
  after :build, :stop
  after :publishing, :start
end