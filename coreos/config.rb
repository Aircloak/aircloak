$num_instances=(ENV['INITIAL_CLUSTER_SIZE'] || "1").to_i

# Change basename of the VM
# The default value is "core", which results in VMs named starting with
# "core-01" through to "core-${num_instances}".
$instance_name_prefix="air"

# Official CoreOS channel from which updates should be downloaded
$update_channel='stable'

$vm_memory = 768

$image_version = "766.4.0"
$enable_serial_logging = false
$share_home = false
$vm_gui = false
$vm_cpus = 1
$shared_folders = {}
$forwarded_ports = {}
