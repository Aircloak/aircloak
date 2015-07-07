namespace :nginx do
  %w[start stop restart].each do |command|
    desc "#{command} nxing"
    task command, roles: :web do
      sudo "/etc/init.d/nginx #{command}"
    end
  end
end
