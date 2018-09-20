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

This test suite runs a set of SQL queries through Aircloak to generate two classes of values:
the real unanonymized (unaltered) values an analyst would get if no Aircloak was used, and a set
of Aircloak anonymized results. These sets of values are then used to determine the data quality
produced by Aircloak, namely how much error does Aircloak introduce as part of the anonymization.

The non-anonymous values are also generated using Aircloak (with a data source where the config
sets the `user_id` to `null` (thereby not anonymizing the data). Using an Aircloak instance for
the non-anonymous data ensures we can run the exact same queries (using functionality such as `bucket`)
against the non-anonymizing data source as the anonymizing ones. This in turn ensures we get values
that can be sensibly compared.

The system can also be configured to run against multiple distinct versions of Aircloak.
This way it can show the changes in results across different versions of Aircloak system.
This is particularly useful when making changes to the anonymization engine where you want
a side-by-side comparison of how these changes affect the data quality.

At present the system measures the mean square error. Future versions might measure things such
as [Pearson's chi-squared test](https://en.wikipedia.org/wiki/Pearson%27s_chi-squared_test) to
measure to what extent the anonymized output are drawn from what seems to be the same distribution.

## Running

### Prerequisites

#### R

You need to have `R` installed. It can be downloaded and installed from [R-project](https://www.r-project.org/)
or with `brew install r` if you are on macOS.
R is used for generating the beta-distribution values that are tested against,
as well as for rendering graphs of the anonymized results.

Run `make setup-r` to install the necessary R libraries that will be needed to run the
data quality tests.

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
