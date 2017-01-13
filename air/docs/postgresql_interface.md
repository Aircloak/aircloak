# PostgreSQL interface

The Air system exposes a PostgreSQL server interface which can be used by to query data sources. The PostgreSQL protocol allows our system to be used by various 3rd party tools and clients, such as Tableau, QlikView, PostgreSQL ODBC driver, PostgreSQL drivers in various languages, ...

The documentation of the protocol is available [here](https://www.postgresql.org/docs/9.6/static/protocol.html). Currently, only a subset of the protocol is implemented. In particular, we support only the following features:

- SSL-based authentication with cleartext password
- Simple queries
- Unnamed extended (parameterized) queries
- Only integer and text data

The queries must be written in AQL. Other SQL features, such as cursors, updates, insertions, creations of tables are currently not supported.

The implementation of the protocol resides in the `Air.PsqlServer.Protocol` module. The TCP/SSL communication is driven by the `Air.PsqlServer.RanchServer` module. These modules are generic, while Air specifics (e.g. authentication, query execution) are provided in the `Air.PsqlServer` module.

## Further work

### Tableau integration

Tableau integration is not yet fully possible. We can connect to the Air from Tableau, but more work is needed to make the system usable. After analyzing queries issued from Tableau, it has been discovered that we need to extend AQL with following features:

- Queryable `pg_catalog` schema
  Tableau (and other similar tools) issues various queries to this schema to get the database structure. We need to somehow handle such queries, and return proper results.
- Support for cursors
  In some cases, Tableau uses cursors to fetch the data. Cursors don't make much sense with AQL, since we need to process all rows to return anonymized results. However, we can still simulate cursors by doing standard query processing, and returning all rows when the `fetch` command is issued.
- Support for schema qualified table identifiers (`"schema_name"."table_name"`)

### Support for language drivers

Another use-case is to allow clients to query data sources from their own code (e.g. Ruby, Python, Java, ...) using available PostgreSQL drivers. This is already possible to some extent, as demonstrated in integration tests, where Elixir code queries cloak using Erlang ODBC client library. However, this functionality should be further tested and extended as needed. We need to add more integration tests (e.g. using `Postgrex` in Elixir, and other drivers in other languages), and if needed extend the implementation.

### Other notable missing features

- When receiving and sending data, we only support integer (`int4` and `int8`), real, `text`, and `boolean` parameters. We need to support additional types, such as `datetime`.
