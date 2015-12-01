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
    current_branch = `git symbolic-ref --short HEAD`.strip
    if current_branch != fetch(:branch)
      puts "Warning: your current branch is:\n  #{current_branch}\n\n"
      puts "But you're deploying the branch:\n  #{fetch(:branch)}\n\n"
      puts "Continue (y/N)?"
      input = $stdin.readline.strip
      exit 1 unless input.upcase == "Y"
    end

    on roles(:build), in: :sequence do
      # update code on the build server
      exec_ml "
            cd #{fetch(:deploy_to)} &&
            git fetch &&
            git checkout #{fetch(:branch)} &&
            git reset --hard origin/#{fetch(:branch)}
          "

      # package docker images
      execute "AIR_ENV=prod REGISTRY_URL=registry.aircloak.com #{fetch(:deploy_to)}/package.sh"

      # rolling upgrade of the cluster
      exec_ml "
            #{fetch(:deploy_to)}/coreos/cluster.sh rolling_upgrade
              #{fetch(:cluster_plugin)}
              #{fetch(:machine_ip)}
          "
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
