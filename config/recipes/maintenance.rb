set_default(:maintenance_path) { "/tmp/hello.maintenance" }

namespace :maintenance do
  desc "Enables maintenance mode for the web app"
  task :start, roles: :app do
    run "touch #{maintenance_path}"
  end

  desc "Disables maintenance mode for the web app"
  task :stop, roles: :app do
    run "rm #{maintenance_path}"
  end
end
