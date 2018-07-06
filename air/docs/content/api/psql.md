## PostgreSQL Message Protocol server

Aircloak Insights allows users to run SQL queries using the [PostgreSQL Message
Protocol](https://www.postgresql.org/docs/current/static/protocol.html). This means that any driver that can be
used to communicate with a PostgreSQL server can also be used to communicate with Aircloak Insights.

__Important note__: While Aircloak Insights understands the PostgreSQL Message Protocol it does not understand
queries that are specific to PostgreSQL. Examples of these are ones which query for metadata related to a PostgreSQL server's
installation, or other PostgreSQL-specific ways of extracting metadata. When using this interface all the same restrictions apply
as when you are executing queries over the web interface or using the [HTTP REST API](/api.md).
You might get unexpected responses when using BI-tools that write and execute their own queries and
are expecting certain PostgreSQL-specific features and behaviours to be present.

In order to query Aircloak Insights using the PostgreSQL Message Protocol server, your Aircloak Insights platform
installation needs to have been configured to allow this. Specifically this means the following:

- The [Insights Air docker container](ops/installation.html#insights-air) must make the PostgreSQL Message Protocol
  server port available
- The firewall configuration needs to allow access to the port from your network context

If you have the [psql](http://postgresguide.com/utilities/psql.html) utility installed on your machine and your
Aircloak Insights user account has access to a data source then you can quickly verify if the setup is working
or not with the following command:

```
psql -h <INSIGHTS-AIR-HOST> -p <PSQL-PORT> -d <DATA-SOURCE-NAME> -U <USER-NAME>
```

Where:

- `<INSIGHTS-AIR-HOST>` is the same domain you use when accessing the Insights Air interface in your browser or
  when using the HTTP REST API. Alternatively you can also use the IP-address of the Insights Air host if no
  domain name has been configured.
- `<PSQL-PORT>` is the port configured during installation of Insights Air. The default value is `8432`.
- `<DATA-SOURCE-NAME>` is the name of the data source you want to query. It is the same as the name used in the list of
  available data sources you are shown when accessing the Insights Air web interface.
- `<USER-NAME>` is the email address you use as your username when logging into the Insights Air platform.

If you are prompted for your password when running this command, and subsequently see a prompt along the lines of what
is shown below, then everything works as expected.

```
$ psql -h aircloak.example.com -p 8432 -d my-sensitive-data -U user@example.com
Password for user user@example.com:
psql (X.Y.Z, server A.B.C)
Type "help" for help.

my-sensitive-data=> SHOW TABLES;
     name
--------------
table1
table2
table3
(3 rows)

my-sensitive-data=>
```

__Notice about suppressed values__: Standard SQL doesn't have an exact way to indicate that a specific value was suppressed. `NULL` is used to indicate that a requested value was not available or was not computable. When using the Web UI or the REST API, Aircloak Insights uses `*` to indicate suppressed values. But when using the PSQL interface,
returning `*` (which is a specific character value) would not be valid for non-text fields and `NULL` is returned in those cases.

In order to work around this restriction, you can either cast the specific non-text column to text, ensuring `*` is correctly returned when data was suppressed, or explicitly filter out regular `NULL` values for that column, with an `IS NOT NULL` or any other filter, ensuring any remaining `NULL` values in the result represent data that was suppressed.
