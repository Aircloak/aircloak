# Hardcoded number of maximum instances. We'll start them on-demand one by one.
$num_instances=5

# Change basename of the VM
# The default value is "core", which results in VMs named starting with
# "core-01" through to "core-${num_instances}".
$instance_name_prefix="air"

# Official CoreOS channel from which updates should be downloaded
$update_channel='alpha'

$vm_memory = 768

$image_version = "833.0.0"
$enable_serial_logging = false
$share_home = false
$vm_gui = false
$vm_cpus = 1
$shared_folders = {}
$forwarded_ports = {}
