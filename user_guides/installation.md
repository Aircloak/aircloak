# Installation tutorial

This tutorial describes how to install and configure the Aircloak components.

## General overview

The Aircloak system consists of two components:

- air - the web site which allows analysts to run anonymized queries in a cloak.
- cloak - the anonymizing query engine.

The access to the cloak component should be highly restricted, since this component has complete read access to the sensitive data. The air component doesn't require such privileges, so you can safely run it on another machine, and maybe even in another network, as long as the cloak component can connect to the air server.

To keep things simple, in this tutorial, we'll be running the air and the cloak containers on the same machine. Moreover, we'll use the Aircloak test-only database server image.

## Installation

Before installing components, make sure that the following prerequisites are met:

- Docker 1.8 or higher should be installed on the host machines.
- The user which installs the components is logged into `quay.io` with `docker login` using credentials provided by Aircloak.

### Installing the air component

#### Setting up the air database

Before installing the air component, you need to create the air user and the database on some PostgreSQL server. You can use arbitrary names for the user and the database. The air user requires full privileges to the air database.

In this tutorial we will run a local PostgreSQL server using the `aircloak/air_db` image.
__Note__: this image is meant to be used for test purposes only. Do not run production database server based on this image.

Before starting the test database server, you need to create the configuration file describing the user and the database:

```bash
$ mkdir -p /aircloak/air/db/config/

$ cat << EOF > /aircloak/air/db/config/config.json
  {
    "database": {
      "name": "air",
      "user": "air",
      "password": ""
    }
  }
EOF
```

With these parameters, we can now start the container:

```bash
$ mkdir -p /aircloak/air/db/data

$ docker run -d --name air_db \
  -v /aircloak/air/db/data:/var/lib/postgresql/data \
  -v /aircloak/air/db/config:/runtime_config \
  -p 5432:5432 \
  quay.io/aircloak/air_db:latest
```

During the first boot, the air user and database will be created, using values from `config.json`. You can verify if everything is fine by checking the container log with `docker logs air_db`.

#### Configuration

With database in place, we can create the air configuration by running the following commands:

```bash
$ mkdir -p /aircloak/air/config/

$ cat << EOF > /aircloak/air/config/config.json
  {
    "site": {
      "auth_secret": auth_secret,
      "endpoint_key_base": endpoint_key_base,
      "api_token_salt": api_token_salt,
      "master_password": master_password
    },

    "database": {
      "host": "air_db",
      "port": 5432,
      "ssl": false,
      "name": "air",
      "user": "air",
      "password": ""
    }
  }
EOF
```

The configuration consists of the following settings:

- site secrets (`auth_secret`, `endpoint_key_base`, `api_token_salt` under the `site` key) - These are used to sign and encrypt various data exchanged with the clients. All secrets should consist of at least 64 characters. For example, you can generate a random secret with the following command: `cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1`
- master site password - you will need this password to create the first user in your site
- database settings for the air database

The values chosen above will suffice for this simple demo. In particular the database parameters point to the test-only local database which we configured in the previous step.

#### Running the Air container

Once the database container is running, you can start the air container with the following command:

```bash
docker run -d --name air \
 -v /aircloak/air/config:/runtime_config \
 -p 8080:8080 \
 --link air_db:air_db \
 quay.io/aircloak/air:latest
```

The most important part here is the mapping of the `/aircloak/air/config` folder to the `/runtime_config` folder in the container. This is how you can pass your configuration to the container. At the very least, this folder must contain the `config.json` file.

The air container listens on port 8080 (HTTP). However, it will also serve HTTPS requests on port 8443 if the private key and the certificate are provided in files named `ssl_key.pem` and `ssl_cert.pem` in the configuration folder. In this case, you'll also need to map the port 8443 to the host.

__Note__: the `--link air_db:air_db` is just a quick hack to link the air container to the air database. You should normally run the air database server on a separate machine, in which case you won't need to link air to other containers.

If everything was properly configured, you should be able to access air on port 8080, and create the administrator user using the master password provided in the `config.json`. In the case of problems, you can check log with `docker logs air`.

### Installing the cloak component

Once the air component is setup, we need to create the configuration for the cloak component:

```bash
mkdir -p /aircloak/cloak/config/

cat << EOF > /aircloak/cloak/config/config.json
  {
    "air_site": "ws://air:8080",
    "data_sources": [
      {
        "driver": "postgresql",
        "parameters": {
          "hostname": "air_db",
          "username": "air",
          "database": "air",
          "password": ""
        },
        "tables": {
          "users": {
            "db_name": "users",
            "user_id": "id",
            "ignore_unsupported_types": false
          }
        }
      }
    ]
  }
EOF
```

The `air_site` parameter holds the address of the air site. In this particular case, the provided address (`air`) will be available, because we'll link the cloak container to the air container.

In the `data_sources` section we're specifying databases and tables which need to be open to analysts for querying. In this tutorial, we're keeping things simple by using `air_db` as the single queryable database.

With this configuration specified, we can start the cloak container as:

```bash
docker run -d --name cloak \
  -v /aircloak/cloak/config:/runtime_config \
  --link air_db:air_db \
  --link air:air \
  quay.io/aircloak/cloak:latest
```

Similarly to the air component, we need to map the configuration folder to `/runtime_config`.

__Note__: Just like with the air container, here we're using links only to quickly make the air and air_db containers accessible to the cloak.

Assuming everything was setup properly, the cloak should be visible in the air system. You can open the local air site in your browser, and verify that the data source is displayed in the list of data sources. In the case of problems, you can check cloak log by running `docker logs cloak`.
