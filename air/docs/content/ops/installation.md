# Overview

The Aircloak system consists of two components:

- Insights Air
- Insights Cloak

For an explanation of these components, see [Components provided by Aircloak](/components.md#components-provided-by-aircloak).

These components should run on separate machines. In addition, the access to the Insights Cloak component should be highly restricted, since this component has complete read access to the sensitive data. The Insights Air component does not require such privileges, so you can optionally run it in another network, as long as Insights Cloak can connect to the Insights Air.

Before installing components, make sure that the following prerequisites are met:

- Docker 1.11 or higher is installed on the host machines.
- The user which installs the components is logged into `quay.io` with `docker login` using credentials provided by Aircloak.
- You have your Aircloak provided customer token available.
- A database in a PostgreSQL server running version 9.4 or higher.
- The Insights Air component requires at least 2GB of RAM.
- The Insights Cloak component requires at least 8GB of RAM. However, for more complex queries on a larger dataset, more memory might be needed.

{% include "../important-notice.md" %}

## Insights Air

Before installing Insights Air, you need to setup the following on your PostgreSQL server:

- a database user for the air component
- a database to which the user has full privileges

You can use arbitrary names for the user and the database.

With the database in place, you can configure the component as explained in the [Configuration guide](./configuration.md#insights-air-configuration).


Before starting the container, make sure to update the image to the latest version:

```bash
docker pull quay.io/aircloak/air:latest
```

If you want to explicitly control which version you're fetching, then provide the explicit version number instead of the `latest` tag:

```bash
docker pull quay.io/aircloak/air:X.Y.Z
```

Once air is properly configured you can start the air container with the following command:

```bash
docker run -d --name air -h air \
  -v configuration_folder:/runtime_config \
  -p desired_http_port:8080 \
  -p desired_https_port:8443 \
  -p desired_postgresql_interface_port:8432 \
  -p desired_monitoring_port:8081 \
  --restart=unless-stopped \
  quay.io/aircloak/air:latest
```

In the command above, the `configuration_folder` is the absolute path to the folder where `config.json` is residing. We propose to choose the path `/aircloak/air/config` for this folder, but you are free to choose any other path.

The `desired_http_port` and `desired_https_port` parameters are the ports you want to expose for HTTP and HTTPS requests. You need to expose at least one of these ports to make the site accessible from the outside. See the [Configuration guide](configuration.md#web-site-configuration) for more details on how to configure HTTP and HTTPS.

The `desired_postgresql_interface_port` should be the port on which you want the system to accept PostgreSQL requests. If you don't want the component to support this interface, you can omit this port mapping. See [Insights Air PostgreSQL interface configuration](configuration.md#insights-air-postgresql-interface-configuration) for more details.

The `desired_monitoring_port` specifies the port which can be used to access the monitoring endpoint. If you don't want the component to expose this endpoint, you can omit this port mapping. See the [Monitoring guide](monitoring.md) for more details.

The `--restart=unless-stopped` option specifies a restart policy which ensures that the container is restarted should it crash. See [here](https://docs.docker.com/engine/reference/run/#restart-policies-restart) for a more detailed explanation.

If everything was properly configured, you should be able to access Insights Air on the configured port, and create the administrator user using the master password provided in the `config.json` file. In the case of problems, you can check the logs with `docker logs air`.

If you want to have explicit control of the component version, replace the `latest` tag in the command with the particular version number.

## Insights Cloak

First, you need to configure the component, as explained in the [Configuration guide](configuration.md#insights-cloak-configuration).

Before starting the container, make sure to update the image to the latest version:

```bash
docker pull quay.io/aircloak/cloak:latest
```

If you want to explicitly control which version you're fetching, then provide the explicit version number instead of the `latest` tag:

```bash
docker pull quay.io/aircloak/cloak:X.Y.Z
```

Assuming the component is configured, we can start the cloak container as:

```bash
docker run -d --name cloak -h cloak \
  -v configuration_folder:/runtime_config \
  --restart=unless-stopped \
  quay.io/aircloak/cloak:latest
```

In the command above, you need to replace `configuration_folder` with the full path to the folder where `config.json` is residing. We propose to choose the path `/aircloak/cloak/config` for this folder, but you are free to choose any other path. If you want to have explicit control of the component version, replace the `latest` tag in the command with the specific version number.

In the case of problems, you can examine logs by running `docker logs cloak` or `docker logs air`, depending on which part of the system you are troubleshooting. In addition, you can enable debug log messages for the cloak component by including `"debug": true` in cloak `config.json` file. You need to restart the component after you change the configuration file.
