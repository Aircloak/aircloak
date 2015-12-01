role :build, %w{root@acdbuild.mpi-sws.org}
role :balancer, %w{root@aclb0.mpi-sws.org}
set :cluster_plugin, "air_prod"
set :machine_ip, "139.19.208.133"
set :balancer_service, "air_balancer-prod"