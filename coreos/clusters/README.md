# Cluster types

This folder contains plugins to manage different cluster types, such as local (vagrant based), staging, production, ...

If you want to create a new cluster type, for example to support some client-specific setup, you need to do the following:

1. Create another folder under this folder. The name of the folder will represent the cluster type.
1. Create the file `cluster_type_folder/plugin.sh` where `cluster_type_folder` is the folder created in the previous step. The script must contain the mandatory function `etcd_settings` which prints the content of etcd config script to stdout (except for the secrets, such as passwords).
1. Create the folder `cluster_type_folder/secrets/` where you'll keep secrets. Make sure to add this folder to `.gitignore` to avoid committing secrets to the repo.
1. Create the folder `cluster_type_folder/secrets/ca` with required keys and certificates.
1. If you need to set some secrets in `etcd`, create the file `cluster_type_folder/secrets/etcd` with corresponding `etcd_set` statements. These statements will be appended to the ones produced by `etcd_settings` function (step 2).
1. If you need to perform some specific actions on the cluster machine, you can add the function `prepare_machine` in `cluster_type_folder/plugin.sh`. This function takes one parameter, the machine's IP address, and is invoked before the installation is started. You can invoke custom ssh/scp commands here to perform required actions.