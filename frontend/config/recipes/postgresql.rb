set_default(:postgresql_host) { Capistrano::CLI.ui.ask "PostgreSQL database server: " }
set_default(:postgresql_database) { Capistrano::CLI.ui.ask "PostgreSQL database: " }
set_default(:postgresql_user) { Capistrano::CLI.ui.ask "PostgreSQL user: " }
set_default(:postgresql_password) { Capistrano::CLI.password_prompt "PostgreSQL Password: " }

namespace :postgresql do
  desc "Generate the database.yml configuration file."
  task :setup, roles: :app do
    run "mkdir -p #{shared_path}/config"
    template "postgresql.yml.erb", "#{shared_path}/config/database.yml"
  end
  after "deploy:setup", "postgresql:setup"

  desc "Symlink the database.yml file into latest release"
  task :symlink, roles: :app do
    run "ln -nfs #{shared_path}/config/database.yml #{release_path}/config/database.yml"
  end
  after "deploy:finalize_update", "postgresql:symlink"
end
