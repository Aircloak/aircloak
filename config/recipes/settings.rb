namespace :settings do
  desc "Symlink the settings.local.yml file into latest release"
  task :symlink, roles: :app do
    run "ln -nfs #{shared_path}/config/settings.local.yml #{release_path}/config/settings.local.yml"
  end
  after "deploy:finalize_update", "settings:symlink"
end