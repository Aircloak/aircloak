# DataQuality

Produces a data quality score for an Aircloak instance.

- [What it does](#what-it-does)
- [Running](#running)
  - [Prerequisites](#prerequisites)
    - [R](#r)
    - [Database](#database)
    - [Config](#config)
    - [Running multiple Aircloak instances locally](#running-multiple-aircloak-instances-locally)
    - [Configuring data sources](#configuring-data-sources)
  - [Running data quality tests](#running-data-quality-tests)

## What it does

When run as `mix data_quality` the script will connect to the data sources
configured in the `config.json` file and execute a set of SQL queries in order
to extract anonymized results. Additionally the same queries are run against a non-anonymizing
version of the data source in order to generate comparison values that the anonymized
values can be compared against. This allows us to generate a measure of how much the
anonymization distorts the expected database output, and based on that produce a
data quality measure.

The final output is a set of mean squared error results per query, distribution and
data source. The query results are also saved as CSV in the `output`-folder.

## Running

### Prerequisites

#### R

You need to have `R` installed. It can be downloaded and installed from [R-project](https://www.r-project.org/).
R is used for generating the beta-distribution values that are tested against,
as well as for rendering graphs of the anonymized results.

#### Database

TL;DR: run `make recreate-db`

You need a locally running Postgres database populated with test data in order to run the data quality tests.
The `recreate-db` make-target will create the required user, database and data for the test.
It requires that the default user used when calling the `psql` command without specifying a user
has sufficient privileges to create new super users.

#### Config

The `config.json` file needs to be configured for your local setup.
Unfortunately we are dependent on using the Air HTTP API for querying
which in turn means you will have to manually export a API token from the
web interface.

The config takes the following form:

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "user": "data_quality",
    "database": "data_quality"
  },
  "raw": <data_source_config>,
  "anonymized": [<data_source_config>]
}
```

Where the `database` are the credentials needed for populating the raw database with data,
and `<data_source_config>` takes the form:

```json
{
  "name": "<name>",
  "host": "<url for web instance>",
  "data_source_name": "<data source name>",
  "api_token": "<api token>"
}
```

The `anonymized` list can contain any number of different Aircloaks. This is useful
for comparing the quality between mutliple distinct versions of our system.

#### Running multiple Aircloak instances locally

In order to be able to run multiple Aircloak instances locally, you can alter
the port numbers the `air` instance listens too. You will have to adapt the
`cloak`-config as well, so it connects to the correct `air`-instance.

Alternatively you could also run different versions of `cloak` all pointing to the same
`air`. This should also work, depending on whether there are major differences to `air`
that have been done between the versions.

#### Configuring data sources

Each cloak instance should offer up an anonymized version of the database.
The data source config could look like this:

```json
{
  "driver": "postgresql",
  "name": "DataQualityAnonymized",
  "parameters": {
    "hostname": "localhost",
    "username": "data_quality",
    "database": "data_quality"
  },
  "tables": {
    "data_quality": {
      "user_id": "uid"
    }
  }
}
```

__One of the cloak's__ also needs to serve up the same data source in unanonymized form.
This is necessary in order to be able to create the raw results the anonymized ones are
compared against. Such a data source configuration could look like:

```json
{
  "driver": "postgresql",
  "name": "DataQualityRaw",
  "parameters": {
    "hostname": "localhost",
    "username": "data_quality",
    "database": "data_quality"
  },
  "tables": {
    "data_quality": {
      "user_id": null
    }
  }
}
```

### Running data quality tests

Running `mix data_quality` will run the data quality tests.
The first time you have to run it with the `--generate-data` flag in order
to generate the data for the tests.
