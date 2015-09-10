# config valid only for Capistrano 3.2.1
lock '3.2.1'

load "config/check.rb"

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
    current_branch = `git symbolic-ref --short HEAD`.strip
    if current_branch != fetch(:branch)
      puts "Warning: your current branch is:\n  #{current_branch}\n\n"
      puts "But you're deploying the branch:\n  #{fetch(:branch)}\n\n"
      puts "Continue (y/N)?"
      input = $stdin.readline.strip
      exit 1 unless input.upcase == "Y"
    end

    [
      "update_code",
      "build_images",
      "provision",
      "update_running_system"
    ].each do |task|
      Rake::Task["aircloak:#{task}"].invoke
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
    end
  end

  task :build_images do
    on roles(:build), in: :sequence do
      [:balancer, :router, :backend, :frontend].each do |service|
        execute "AIR_ENV=prod #{fetch(:deploy_to)}/#{service}/build-image.sh"
      end
      execute ". #{fetch(:deploy_to)}/common/docker_helper.sh && cleanup_unused_images || true"
      execute ". #{fetch(:deploy_to)}/common/docker_helper.sh && print_most_recent_versions"
    end
  end

  task :provision do
    on roles(:app_admin), in: :sequence do
      exec_ml "
            cp -rp /aircloak/air/remote_files/air /etc/init.d/air &&
            chown root:root /etc/init.d/air &&
            chmod 755 /etc/init.d/air &&
            update-rc.d air defaults
          "
    end
  end

  task :update_running_system do
    on roles(:app), in: :sequence do
      execute "/etc/init.d/air start"
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
    #           bash -c 'RAILS_ENV=production bundle exec rake db:migrate'
    #       "
    def exec_ml(cmd)
      execute cmd.gsub("\n", " ")
    end
end
