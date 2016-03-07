app_dir = "/aircloak/website"

working_directory app_dir

# This directory should be mounted on the docker host
pid "#{app_dir}/tmp/unicorn.pid"

# The local nginx forwards request to this socket
listen "/tmp/air-rails.sock"

timeout 180

worker_processes 8
preload_app true

before_fork do |server, worker|
  # The following is highly recomended for Rails + "preload_app true" as
  # there's no need for the master process to hold a connection.
  defined?(ActiveRecord::Base) and ActiveRecord::Base.connection.disconnect!
end
