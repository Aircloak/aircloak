# Individual components

An Aircloak installation consists of a set of individual components. A subset of these are provided by Aircloak. The
remaining ones are provided by the customer. Below follows a description of the individual components.

## Components provided by Aircloak

### Insights Air

Insights Air is the component that analysts and system administrators interact with directly. It provides a web
interface for managing users, user privileges, and datasources, as well as for running queries. Additionally Insights Air
provides an [HTTP API](api.md) for connecting external tools to an Aircloak installation as well as an endpoint for tools and
plugins that support the [PostgreSQL message protocol](api/psql.md).

Insights Air never handles sensitive user data. It can therefore safely be deployed in
a [DMZ](https://en.wikipedia.org/wiki/Perimeter_Network) or made available to less
privileged users.

### Insights Cloak

Insights Cloak analyses and anonymizes sensitive data as requested by Insights Air. It operates on raw and
sensitive data and should therefore run in an environment that is well protected. Insights Cloak does not require the
ability to receive inbound connections. Upon booting it will establish a connection to the Insights Air instance it has
been statically configured to trust. Multiple Insights Cloak instances can all be connected to the same Insights Air
instance, and furthermore do not communicate with each other.

It is highly recommended that Insights Cloak is hosted within a restricted and well protected network as it has access
to and operates on sensitive and restricted data.

_Only anonymized and aggregated data is sent from Insights Cloak to Insights Air._

### Insights Datasource Connector

Insights Datasource Connector knows how to provide the data required by Insights Cloak to run a query. It connects
to the database server hosting the datasource being queried and transfers the required data to Insights Cloak. Insights
Datasource Connector, like Insights Cloak, does not permanently store any sensitive data.

Insights Datasource Connector is deployed and configured as part of Insights Cloak. An Insights Datasource Connector
instance therefore only serves a single Insights Cloak. An Insights Cloak instance on the other hand may make use of
multiple distinct Insights Datasource Connectors in order to serve data from distinct datasources.

Because Insights Datasource Connector needs access to the database server hosting the data to be analysed,
so does the Insights Cloak that it is a part of.

The Insights Datasource Connector has the ability to emulate database features beyond what is supported natively by the
database server itself. Examples of this include the ability to work on encrypted fields and columns, converting data to
types not natively supported, and performing table joins where no such support exists.

For more information about the supported datastores and what query features are emulated, please have a look at the
[datastore](datastores.md) page.

## Components provided by the customer

### PostgreSQL database

Insights Air requires access to a PostgreSQL database. This database is used to store analyst accounts, system settings,
as well as audit logs. No sensitive user data nor access credentials to the datasources being queried are stored in this
database.

### Datasource

The data being queried is made available through a database system of some kind. In nearly all cases this system already
exists and can directly be used by the Aircloak Insights platform.

A list of supported datasources can be found [here](datastores.md).


### Logging infrastructure

Insights Air, and Insights Cloak (and through Insights Cloak also Insights Datasource Connector) produce logs that can
be used to determine if the system is behaving as expected. These logs are automatically collected by the infrastructure
on which the individual components run, but can optionally be forwarded to a centralized log storage and processing
facility operated by the customer.

### Monitoring

Insights Air provides a [monitoring API endpoint](monitoring.html). This API endpoint can be tied into existing monitoring infrastructure, to improve
the visibility into the workings of the Aircloak Insights platform.


## How the components interact

The following diagram shows the possible ways in which the components interact. For common configurations, see the
[deployment guide](deployment.md).

![Shows an overview of different ways in which the Aircloak Insights components interact. IA: Insights Air, IC: Insights
Cloak, IDC: Insights Datasource connector, DS: Datasource](components/interactions.png)

In the diagram above the arrows point to the component being connected to.
For example it shows that Insights Cloak connects to Insights Air,
rather than the other way around.

Of special interest is that:

- An Insights Air instance can serve multiple Insights Cloak instances
- An Insights Cloak instance can connect to multiple datasources through
  multiple Insights Datasource Connectors. These datasources might be hosted in a
  single or in distinct database servers.
- The Insights Cloak and Insights Datasource Connectors should be hosted in a
  protected environment, whereas it is generally safe to allow analysts outside
  the customer organisation to access Insights Air
