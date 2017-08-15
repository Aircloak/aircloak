# Version 17.4.0

## Insights Air

- HTTPS configuration

  Previously, HTTPS was configured by simply placing the files named `ssl_cert.pem` and `ssl_key.pem` into the same folder as the `config.json` file. Starting with this version, the file names must be explicitly provided in the `config.json` file under the `site` key:

  ```
  # config.json

  {
    "site": {
      "certfile": "ssl_cert.pem",
      "keyfile": "ssl_key.pem",
      ...
    },
    ...
  }
  ```

  These keys are optional, and if they are not provided, the system will only serve HTTP traffic.

  For more details about site configuration, see [here](configuration.md#web-site-configuration).

- SSL configuration for Insights Air PostgreSQL interface

  Similarly to the site settings, the PostgreSQL server certificate and key file names must be explicitly provided. These files can be configured under the `psql_server` key:

  ```
  # config.json

  {
    "psql_server": {
      "certfile": "ssl_cert.pem",
      "keyfile": "ssl_key.pem",
      ...
    },
    ...
  }
  ```

  For more details about Insights Air PostgreSQL interface, see [here](configuration.md#insights-air-postgresql-interface-configuration).

- TCP configuration for Insights Air PostgreSQL interface

  Starting with this version, it is possible to accept PostgreSQL connections over plain TCP (previously only SSL connections were supported). In order to allow TCP connections, you must explicitly set the value of `require_ssl` under the `psql_server` key to `false`:

  ```
  # config.json
  {
    "psql_server": {
      "require_ssl": false,
      ...
    },
    ...
  }
  ```

  For more details about Insights Air PostgreSQL interface, see [here](configuration.md#insights-air-postgresql-interface-configuration).
