# Installation tutorial

This tutorial describes how to install and configure Aircloak components.

## General overview

The Aircloak system consists of two components:

- cloak - the query engine of the Aircloak system.
- air - the web site which allows analysts to run anonymized queries in a cloak.

In general, you should run each component on a separate machine, and in different networks. In particular, the access to the cloak component should be highly restricted, since this component has complete read access to the sensitive data.

To keep things simple, in this tutorial, we'll be running air and cloak containers on the same machine. Moreover, to keep things simple, we'll use Aircloak test-only database server image.

## Installation

Before installing components, make sure that the following prerequisites are met:

- Docker 1.8 or higher should be installed on the host machines.
- You have a [quay](quay.io) account with access to Aircloak repositories.
- The user which installs the components is logged into quay with `docker login`.

### Installing the air component

#### Setting up the air database

Before installing the air component, you need to create the air user and the database on some PostgreSQL server. You can use arbitrary names for the user and the database. The air user requires full privileges to the air database.

In this tutorial you can skip this particular step. Aircloak provides a database image to easily start a local PostgreSQL server. Keep in mind that __this image is meant to used for test purposes only.__

#### Configuration

With database in place, we can create the air configuration. We'll keep it in the `/aircloak/air/config` folder. To create the configuration, you can run the following commands:

```bash
$ mkdir -p /aircloak/air/config/

$ cat << EOF > /aircloak/air/config/config.json
  {
    "site": {
      "auth_secret": "NoTvrYAq3tAuiC0edW8X/Xb2yeSQ/kc0CyUibLM8zLIqCg5j7+TxaU5B0P8Q90o8",
      "endpoint_key_base": "sKMa8Cd7n3ul4ARLf17Vz/jdC28XlLd3VNS2Dnpf+wDLzF3US7hy3eJ/O/K2Cg2o",
      "api_token_salt": "3GYnplkbe2m6pop/q1nOXLGzGhnsi/JjHebzEY3epOi7RHct/A4RIxbQtravVQBH",
      "master_password": "bNuvH4FflZ10"
    },

    "database": {
      "host": "air_db",
      "port": 5432,
      "ssl": false,
      "name": "air",
      "user": "air",
      "password": "Z4zFkBm9sCPs"
    }
  }
EOF
```

The configuration consists of the following settings:

- site secrets (`auth_secret`, `endpoint_key_base`, `api_token_salt` under the `site` key) - These are used to sign and encrypt various data exchanged with the clients. A secret should consist of at least 64 characters
- master site password - you will need this password to create the first user in your site
- database settings for the air database

The values chosen above will suffice for this simple demo. In particular the database parameters point to the test-only local database (which we'll start shortly).

When configuring a real-life system you should generate your own secrets and the master site password, and provide correct database settings.

#### Running a test-only local air database

The test-only database server is provided through the `quay.io/aircloak/air_db` docker  image. Using this image, you can start the container with the following command:

```bash
$ mkdir -p /aircloak/air/db

$ docker run -d --name air_db \
  -v /aircloak/air/db:/var/lib/postgresql/data \
  -v /aircloak/air/config:/runtime_config \
  -p 5432:5432 \
  quay.io/aircloak/air_db:latest
```

The container will create the air user and database as described by the `config.json`. Notice how we're mapping the PostgreSQL data folder to the host's `/aircloak/air/db` folder to keep the data persistent.

#### Running the Air container

Once the database container is running, you can start the air container with the following command:

```bash
docker run -d --name air \
 -v /aircloak/air/config:/runtime_config \
 -p 8080:8080 \
 --link air_db:air_db \
 quay.io/aircloak/air:latest
```

The most important part here is the mapping of the `/aircloak/air/config` folder to the `/runtime_config` folder in the container. This is how you can pass your configuration to the container. At the very least, this folder must contain the `config.json` file described earlier.

The air container listens on port 8080 (HTTP). However, it will also serve HTTPS requests on port 8443 if the private key and the certificate are provided in files named `ssl_key.pem` and `ssl_cert.pem` in the configuration folder. In this case, you'll also need to map the port 8443 to the host.

__Note__: the `--link air_db:air_db` is just a quick hack to link the air container to the air database. As already mentioned, you should normally run the air database server on a separate machine, in which case you won't need to link air to other containers.

If everything was properly configured, you should be able to access air on port 8080, and create the administrator user using the master password provided in the `config.json`.

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
          "password": "Z4zFkBm9sCPs"
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

Assuming everything was setup properly, the cloak should be visible in the air system.
