# config valid only for Capistrano 3.2.1
lock '3.2.1'

set :application, 'air'
set :branch, ENV["AIR_DEPLOY_BRANCH"] || "develop"
set :deploy_to, '/aircloak/air'

# Prevents key hijacking.
set :ssh_options, {:forward_agent => false}

# We override the standard deploy workflow, since we're building containers here.
# Thus, we perform deploy always in the same folder. We update the code, build
# images, and then restart docker containers.
Rake::Task["deploy"].clear_actions

task :deploy do
  Rake::Task["aircloak:deploy"].invoke
end

namespace :aircloak do
  task :deploy do
    Rake::Task["aircloak:update_code"].invoke
    Rake::Task["aircloak:backend:build"].invoke
    Rake::Task["aircloak:frontend:build"].invoke
    Rake::Task["aircloak:db_migrate"].invoke
    Rake::Task["aircloak:restart"].invoke
  end

  task :db_migrate do
    on roles(:app), in: :sequence do
      exec_ml "
            cd #{fetch(:deploy_to)}/frontend &&
            docker run -t --rm
              -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log
              aircloak/air_frontend:latest
              bash -c 'ETCD_HOST=172.17.42.1 ETCD_PORT=4002 RAILS_ENV=production bundle exec rake db:migrate'
          "
    end
  end

  task :update_code do
    on roles(:build), in: :sequence do
      exec_ml "
            cd #{fetch(:deploy_to)} &&
            git fetch &&
            git checkout #{fetch(:branch)} &&
            git reset --hard origin/#{fetch(:branch)}
          "

      exec_ml "
            mkdir -p #{fetch(:deploy_to)}/frontend/log &&
            chmod 777 #{fetch(:deploy_to)}/frontend/log
          "

      exec_ml "
            mkdir -p #{fetch(:deploy_to)}/frontend/var-log &&
            chmod 777 #{fetch(:deploy_to)}/frontend/var-log
          "
    end
  end

  task :restart do
    on roles(:app), in: :sequence do
      execute "/etc/init.d/air start"
    end
  end

  namespace :backend do
    task :build do
      on roles(:build), in: :sequence do
        execute "AIR_ENV=prod #{fetch(:deploy_to)}/backend/build-image.sh"
      end
    end
  end

  namespace :frontend do
    task :build do
      on roles(:build), in: :sequence do
        execute "AIR_ENV=prod #{fetch(:deploy_to)}/frontend/build-image.sh"
      end
    end
  end

  private
    # Capistrano execute replaces new-lines with a semicolon. In this wrapper,
    # we simply insert blanks instead, thus allowing a multi-line command, e.g.:
    #
    #   exec_ml "
    #         docker run -t --rm
    #           -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log
    #           aircloak/air_frontend:latest
    #           bash -c 'ETCD_HOST=172.17.42.1 ETCD_PORT=4002 RAILS_ENV=production bundle exec rake db:migrate'
    #       "
    def exec_ml(cmd)
      execute cmd.gsub("\n", " ")
    end
end
