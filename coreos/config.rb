# Hardcoded number of maximum instances. We'll start them on-demand one by one.
$num_instances=5

# Change basename of the VM
# The default value is "core", which results in VMs named starting with
# "core-01" through to "core-${num_instances}".
$instance_name_prefix="air"

# CoreOS channel from which updates should be downloaded
$update_channel='stable'

# Determine latest CoreOS version from the specified channel
require 'open-uri'
most_recent_version =
  open("http://#{$update_channel}.release.core-os.net/amd64-usr/").
      read.
      scan(/href="(.*?)\/"/)[-2][0]

puts "Starting with CoreOS #{most_recent_version}" if ARGV[0] == "up"

$vm_memory = 768
$image_version = most_recent_version
$enable_serial_logging = false
$share_home = false
$vm_gui = false
$vm_cpus = 1
$shared_folders = {}
$forwarded_ports = {}
