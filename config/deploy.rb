# config valid only for Capistrano 3.2.1
lock '3.2.1'

set :application, 'air'
set :branch, ENV["AIR_DEPLOY_BRANCH"] || "develop"

# Prevents key hijacking.
set :ssh_options, {:forward_agent => false}

# We override the standard deploy workflow, since we're building containers here.
# Thus, we perform deploy always in the same folder. We update the code, build
# images, and then restart docker containers.
Rake::Task["deploy"].clear_actions

task :deploy do
  Rake::Task["aircloak:deploy"].invoke
end

task :upgrade_balancer do
  Rake::Task["aircloak:upgrade_balancer"].invoke
end

namespace :aircloak do
  task :deploy do
    check_current_branch

    on roles(:build), in: :sequence do
      update_server_code

      # pull the latest version of the static website.
      exec_ml "
            cd #{build_folder}/../static-website &&
            git fetch &&
            git checkout develop &&
            git reset --hard origin/develop
          "

      # package docker images
      exec_ml "
            AIR_ENV=prod
            REGISTRY_URL=registry.aircloak.com
            IMAGE_CATEGORY=#{fetch(:stage)}
            #{build_folder}/package.sh
          "

      # rolling upgrade of the cluster
      exec_ml "
            #{build_folder}/coreos/cluster.sh rolling_upgrade
              #{fetch(:stage)}
              #{fetch(:machine_ip)}
          "
    end
  end

  task :upgrade_balancer do
    check_current_branch

    on roles(:balancer), in: :sequence do
      update_server_code

      # build the balancer image
      execute "AIR_ENV=prod #{build_folder}/balancer/build-image.sh"

      # restart the systemd service
      execute "systemctl daemon-reload && systemctl restart #{fetch(:balancer_service)}"
    end
  end

  private
    def build_folder
      "/aircloak/#{fetch(:stage)}/air"
    end

    def check_current_branch
      current_branch = `git symbolic-ref --short HEAD`.strip
      if current_branch != fetch(:branch)
        puts "Warning: your current branch is:\n  #{current_branch}\n\n"
        puts "But you're deploying the branch:\n  #{fetch(:branch)}\n\n"
        puts "Continue (y/N)?"
        input = $stdin.readline.strip
        exit 1 unless input.upcase == "Y"
      end
    end

    def update_server_code
      exec_ml "
            cd #{build_folder} &&
            git fetch &&
            git checkout #{fetch(:branch)} &&
            git reset --hard origin/#{fetch(:branch)}
          "
    end

    # Capistrano execute replaces new-lines with a semicolon. In this wrapper,
    # we simply insert blanks instead, thus allowing a multi-line command, e.g.:
    #
    #   exec_ml "
    #         docker run -t --rm
    #           -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log
    #           aircloak/air_frontend:latest
    #           bash -c 'RAILS_ENV=production bundle exec rake db:migrate'
    #       "
    def exec_ml(cmd)
      execute cmd.gsub("\n", " ")
    end
end
