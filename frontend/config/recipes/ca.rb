namespace :ca do
  desc "Symlink the ca folder into latest release"
  task :symlink, roles: :app do
    run "mkdir -p #{shared_path}/config/ca"
    run "ln -nfs #{shared_path}/config/ca #{release_path}/config/ca"
  end
  after "deploy:finalize_update", "ca:symlink"
end