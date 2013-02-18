set_default(:postgresql_host) { Capistrano::CLI.ui.ask "PostgreSQL database server: " }
set_default(:postgresql_password) { Capistrano::CLI.password_prompt "PostgreSQL Password: " }
set_default(:postgresql_user) { "#{application}_production" }
set_default(:postgresql_database) { "#{application}_production" }

namespace :postgresql do
  desc "Generate the database.yml configuration file."

  task :install, roles: :app do
    run "#{sudo} apt-get -y install libpq-dev"
  end
  after "deploy:install", "postgresql:install"

  desc "Symlink the database.yml file into latest release"
  task :symlink, roles: :app do
    run "ln -nfs #{shared_path}/config/database.yml #{release_path}/config/database.yml"
  end
  after "deploy:finalize_update", "postgresql:symlink"
end
