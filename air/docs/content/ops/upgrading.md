# Version 18.3.0

## Insights Cloak

### Static analysis of columns

From version `18.3.0` onwards, Insights Cloak performs an analysis of the columns in a data source when booting.
The analysis determines which columns are likely to isolate users. A data source might contain columns that behave
much like the `user-id` column without explicitly being a `user-id` column. Consider columns containing
email addresses or social security numbers as examples.
Columns that are deemed to be isolating get a set of extra restrictions applied to them.
For more information, please consult the [Isolating columns](/sql/restrictions.md#isolating-columns)
section of the restrictions chapter.

Depending on the database size the static analysis might take quite some time to complete. The Insight Cloak
supports caching the results to avoid having to reperform the analysis when Insights Cloak is restarted or upgraded.
To enable caching you have to mount a folder into the Insights Cloak container under the path `/persist`.
Your `docker run ...` command would have to be updated to look something like this:

```
docker run ... \
  -v cloak_persist_folder:/persist \
  ...
```

Where `cloak_persist_folder` is the path you want the cache to be stored at on your host system.
Depending on your setup it might be something like `/aircloak/cloak/cache`.

__The Cloak container needs both read and write permissions to this folder.__

If the static analysis puts undue stress on your data source, or does not complete within a reasonable time, please
consider manually classifyng your columns. More information on how this is done can be found
[here](configuration.md#insights-cloak-configuration) under the heading _Manually classifying isolating columns_.

# Version 17.5.0

## Insights Cloak

### Datasources configuration

__The old style of configuring datasources will stop working in version 18.3.__

Datasources can now be configured in individiual files to make sharing of datasource configurations easier
between multiple Insights Cloak instances.

In previous versions the `data_sources` section of the Insights Cloak configuration contained an array of datasource
definitions. These individual definitions should now be stored in separate files in a subdirectory of the
directory where the Insights Cloak `config.json` file is stored.

The `data_sources` parameter in the configuration file should now hold the path to the folder containing the individual
configuration files.

Below follows an example of an Insights Cloak config that served two datasources: `data_source1`, and `data_source2`.
This example Insights Cloak has the following configuration stored in a folder called `cloak-configuration`:

__config.json__:

```json
{
  "air_site": "wss://air.example.com:8443",
  "salt": "strong salt",
  "data_sources": [
    {
      "name": "data_source1",
      ...
    },
    {
      "name": "data_source2",
      ...
    }
  ]
}
```

To migrate to the new format, create a subdirectory under `cloak-configuration` called `enabled-datasources`.
In it you create the following two datasource definitions:


__data_source1.json__:

```
{
  "name": "data_source1",
  ...
}
```

__data_source2.json__:
```
{
  "name": "data_source2",
  ...
}
```

and alter your Insights Cloak `config.json` file to read:

```json
{
  "air_site": "wss://localhost:8443",
  "salt": "Salt for local dev cloak",
  "data_sources": "enabled-datasources"
}
```


# Version 17.4.0

## Insights Air

### HTTPS configuration

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

### SSL configuration for Insights Air PostgreSQL interface

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

### TCP configuration for Insights Air PostgreSQL interface

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
