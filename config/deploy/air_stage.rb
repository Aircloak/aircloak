role :build, %w{root@acdbuild.mpi-sws.org}
role :balancer, %w{root@aclb0.mpi-sws.org}
set :machine_ip, "139.19.208.125"
set :balancer_service, "air_balancer-stage"