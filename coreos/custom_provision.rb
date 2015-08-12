require 'fileutils'

module Aircloak
  def self.custom_provision(config)
    # Copy needed scripts from other components to the shared folder
    root_path = File.expand_path("..")
    [
      "../etcd/config_coreos.sh",
      "../etcd/etcd_values_coreos",
      "../etcd/etcd_lib.sh",
      "../common/docker_helper.sh",
      "../frontend/container.sh",
      "../backend/container.sh",
      "../balancer/container.sh"
    ].each do |file|
      target = File.dirname(File.expand_path(file).gsub(root_path, "./shared/air"))
      FileUtils.mkdir_p(target)
      FileUtils.cp(file, target)
    end

    config.vm.synced_folder "./shared/", "/tmp/shared", create: true
    config.vm.provision :shell,
      privileged: true,
      inline: <<-EOF
        DOCKER_REGISTRY_URL='#{ENV['COREOS_HOST_IP']}:5000' \
        DB_SERVER_URL='#{ENV['COREOS_HOST_IP']}' \
        AIRPUB_URL='#{ENV['COREOS_HOST_IP']}:1080' \
        AIR_ENDPOINT='#{ENV['COREOS_HOST_IP']}:8999' \
        /tmp/shared/setup_system.sh
EOF
  end
end