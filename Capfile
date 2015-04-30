require 'capistrano/setup'
require 'capistrano/deploy'
require 'capistrano/git'

module GitStrategy
  # do all the things a normal capistrano git session would do
  include Capistrano::Git::DefaultStrategy

  # Custom release task. We're extracting to the build subfolder. See deploy.rb
  # for details on build.
  def release
    on release_roles :all do
      execute "mkdir -p #{release_path}/build"
    end
    git :archive, fetch(:branch), '| tar -x -C', "#{release_path}/build"
  end
end