air_common application
==========

----------------------

- [What it does](#what-it-does)

----------------------

# What it does

This is the base application used by other OTP applications in the backend system. It provides some common functionalities, such as etcd helper. In addition, it is in charge of establishing cluster of nodes. This is performed by each node through etcd. In particular, each node does following steps:

- Adds its specific entry containing the full node name to etcd.
- Periodically polls the parent directory where nodes are added.
- Connects to all registered node which are not in the cluster.
