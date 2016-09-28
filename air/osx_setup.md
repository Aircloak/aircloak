# OS X and docker-machine

On OS X machine, docker service is running inside docker-machine VM, so some additional setting up is needed.

## Starting and intializing docker-machine

After your localhost is booted, you need to start docker-machine with `docker-machine start default` (assuming your machine is named `default`).

Then, in each shell session you need to run `eval $(docker-machine env default)`

You can consider putting these statements inside `~/.bash_profile`

## Port forwarding

You need to forward following ports from your localhost to docker-machine VM:

- 9080 (air http)
- 9443 (air https)
- 20002 (database dev container)
- 20003 (database test container)

## Mounting non-home folders

If your repository is located outside of your home directory, you need to mount the top most
parent folder to docker-machine VM under the same location. For example, if this repo is locally situated under
`/projects/aircloak`, you need to map your local `/projects` folder to `/projects` on docker-machine VM.

You can use a helper script in the root of this repository to setup an NFS mount:

1. Make sure you have `nfs.server.mount.require_resv_port = 0` in your local `/etc/nfs.conf`
2. Run `sudo ./osx_mount_nfs.sh default LOCAL_MACHINE_IP /projects/`, where `LOCAL_MACHINE_IP` is the network ip address of your machine.
3. Verify that `/projects` is properly mounted on your docker-machine VM via `docker-machine ssh default 'ls -al /projects/'`. If all went well, the listing will correspond to the listing of your local `/project` folder.

__Note__: you need to mount folders after each restart of docker-machine VM.
