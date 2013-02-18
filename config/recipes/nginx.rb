namespace :nginx do
  desc "This task currently does nothing, since we are deploying with passenger. Please install nginx manually"
  task :install, roles: :web do
    puts "INSTALL NGINX ON THE SERVER"
  end
  after "deploy:install", "nginx:install"

  desc "Setup nginx configuration for this application"
  task :setup, roles: :web do
    template "nginx.erb", "/tmp/nginx_conf"
    run "#{sudo} mv /tmp/nginx_conf /etc/nginx/sites-enabled/#{application}"
    run "#{sudo} rm -f /etc/nginx/sites-enabled/default"
    restart
  end
  after "deploy:setup", "nginx:setup"

  %w[start stop restart].each do |command|
    desc "#{command} nxing"
    task command, roles: :web do
      run "#{sudo} /etc/init.d/nginx #{command}"
    end
  end
end
