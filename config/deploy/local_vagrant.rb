# Supports deploying to local vagrant box.
#
# 1. To use, you'll need a Vagrantfile. Here's an example:
#
#   #!/usr/bin/env ruby
#
#   VAGRANTFILE_API_VERSION = "2"
#
#   Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
#     config.vm.box = "Debian 7.3"
#     config.vm.box_url = "http://puppet-vagrant-boxes.puppetlabs.com/debian-73-x64-virtualbox-nocm.box"
#
#
# 2. You also need to add following to you ~/.ssh/config:
#
#   Host vagrant
#     HostName 127.0.0.1
#     User vagrant
#     Port 2222
#     UserKnownHostsFile /dev/null
#     StrictHostKeyChecking no
#     PasswordAuthentication no
#     IdentityFile /Users/sasa/.vagrant.d/insecure_private_key
#     IdentitiesOnly yes
#     LogLevel FATAL
#
# 3. Start the machine, and set up folders and permissions for the vagrant user, as explained in README.md.
#
# 4. Deploy with
#      bundle exec cap local_vagrant deploy

role :app, %w{vagrant@vagrant}