# OS X and boot2docker

On OS X machine, docker service is running inside boot2docker VM, so some additional setting up is needed.

## Starting and intializing boot2docker

After your localhost is booted, you need to start boot2docker with `boot2docker start`

Then, in each shell session you need to run `eval $(boot2docker shellinit 2>/dev/null)`

You can consider putting these statements inside `~/.bash_profile`

## Port forwarding

You need to forward following ports from your localhost to boot2docker VM:

- 20002 (database container)
- 20020 (etcd for local dev)
- 20100 (router container https)
- 20101 (balancer container https)
- 20120 (etcd for docker containers)
- 20126 (docker registry container)
- 20220 (etcd for tests)

## Mounting non-home folders

If your repository is located outside of your home directory, you need to mount the top most
parent folder to boot2docker VM under the same location. For example, if this repo is locally situated under
`/projects/aircloak/web`, you need to map your local `/projects` folder to `/projects` on boot2docker VM.

One way to do this is to share the folder via VirtualBox and auto mount it on boot2docker boot:

1. Add `projects` as the shared folder in VirtualBox under some name (e.g. PROJECTS)
2. Make sure boot2docker VM is started. Run `boot2docker ssh` and create the file `/var/lib/boot2docker/bootlocal.sh` with the following contents:
```
sudo mkdir -p /projects
sudo mount -t vboxsf -o uid=1000,gid=50 PROJECTS /projects
```
3. `chmod +x /var/lib/boot2docker/bootlocal.sh`
4. Exit the machine and run `boot2docker restart`
5. Verify that `/projects` is available on your boot2docker machine.

Alternatively, you can setup an NFS mount. You can use a helper script in the root of this repository:

1. Make sure you have `nfs.server.mount.require_resv_port = 0` in your local `/etc/nfs.conf`
2. Run `./osx_mount_nfs /projects`
3. Verify that `/projects` is available on your boot2docker machine.

__Note__: if using NFS approach, you need to mount folders after each restart of boot2docker VM.
