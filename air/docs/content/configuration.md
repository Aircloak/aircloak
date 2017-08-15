# Overview

Before starting the system, you need to configure the Insights Air and Insights Cloak components. Both components are configured through a file called `config.json`. Each component requires its own `config.json` file which must be placed in a separate folder. In other words, you can't have a single `config.json` for both components. When starting each component, you need to mount the folder containing this file, as explained in the [Installation guide](installation.md).

## Insights Air configuration

The Insights Air configuration needs to provide the following information:

- database connection parameters (required)
- web site configuration (required)
- Insights Air PostgreSQL interface parameters (optional)

The general shape of `config.json` is therefore:

```
{
  "database": {
    ...
  },
  "site": {
    ...
  },
  "psql_server": {
    ...
  },
}
```

### Database configuration

The database configuration is used to specify connection parameters for the database used by the Insights Air component to store data such as users, groups, history of queries, and other. The database has to be hosted on a PostgreSQL server version 9.4 or higher.

This section looks as follows:

```
"database": {
  "host": string,
  "port": integer,
  "ssl": boolean,
  "user": string,
  "password": string,
  "name": string
}
```

### Web site configuration

This part of the configuration is used to configure the web server of the Insights Air component. The shape of this section is as follows:

```
"site": {
  "auth_secret": secret_string,
  "endpoint_key_base": secret_string,
  "api_token_salt": secret_string,
  "customer_token": string,
  "master_password": string,
  "certfile": string,
  "keyfile": string
},
```

In the snippet above, the type `secret_string` indicates a string which should consist of at least 64 characters. The corresponding parameters are used to sign and encrypt various data sent to the client. Make sure to choose values which are random enough, or otherwise the security and privacy of the system might be compromised. For example, to generate a random secret, you can use the following command:

```
cat /dev/urandom |
  LC_CTYPE=C tr -dc 'a-zA-Z0-9' |
  fold -w 64 |
  head -n 1
```

The customer token is the token provided by Aircloak. This value uniquely identifies your Insights Air installation with the central Aircloak systems.

The `master_password` parameter specifies the password (in clear text) which is required when creating the first administrator user. When you attempt to access the site for the very first time, there are no users in the database. At this point, the system will ask you to create the first administrator user, and it will require you to enter the `master_password`. Once the first administrator is created, this password will not be needed anymore.

The final two parameters `certfile` and `keyfile` are optional. They are used to specify the certificate and key for the HTTPS interface. If these parameters are provided, you will also need to put the corresponding files in the same folder as the `config.json` file. Once you do that, the site will accept HTTPS traffic as well as HTTP traffic. If you omit these parameters, the site will only accept HTTP traffic.

The ports on which the site will listen are hardcoded. HTTP traffic is served via port 8080, while HTTPS is served via 8443. As explained in the [Installation guide](installation.md), you can use Docker port mapping option to expose these ports.

We strongly suggest to use only HTTPS for communication between the clients (browsers) and the server (the Insights Air component). Otherwise, the security of the system might be compromised.

### Insights Air PostgreSQL interface configuration

This part of the configuration allows you to instruct the Insights Air component to act as a PostgreSQL server. This can be useful when you want to query the system from third party tools, such as Tableau.

The configuration consists of the following parameters:

```
"psql_server": {
  "require_ssl": boolean,
  "certfile": string,
  "keyfile": string
}
```

The `require_ssl` parameter specifies whether the connection requires all clients to connect over SSL. If this value is `true`, you also need to provide `certfile` and `keyfile` parameters which specify the file names of the certificate and the key. These files need to be placed in the same folder as the `config.json` file.

If `require_ssl` is false, then the server will accept TCP connection as well as SSL. However, if `certfile` and `keyfile` parameters are not provided, then the server will only work with unencrypted TCP connections.

Regardless of which transport protocol(s) are allowed, the server will always accept requests on the port 8432. As explained in the [Installation guide](installation.md), you can use the docker port mapping to expose this port to the outside world.

Once the component is started, you can test the connectivity with the `psql` command line tool:

```
psql -h insights_air_ip_address -p 8432 -d data_source_name -U user_name
```

In order for the above command to work, the cloak component must be started as well, and the user must have permissions to query the given data source.
