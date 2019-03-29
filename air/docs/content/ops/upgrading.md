# Version 18.5.0

## Insights Air

### Change in monitoring report format

The format of the reported memory stats in the monitoring API endpoint has changed.
Please consult the [monitoring](/ops/monitoring.md) guide for information on the new
format.

# Version 18.4.0

## Insights Air

### Altered privacy policy flow

The privacy policy flow has been changed. Where new users were previously required to accept the privacy policy
before they could start using the system, they now only have the privacy policy available for perusal at their
own pleasure. From version 18.4.0 it is the responsibility of the organisation hosting the Aircloak Insights
installation to inform their analysts about the privacy policy and their rights.

The default content has also been updated to reflect these changes. You can see the updated default content here:
[Privacy policy default content](upgrade/1804_privacy_policy.md).

### Non-docker deployments

If you are running Aircloak Insights without the use of docker containers, you will now have to provide a
second PostgreSQL database server instance. More details can be found in the [Running without Docker containers](/ops/configuration.html#running-without-docker-containers)
section of the operations guides.

# Version 18.3.0

## Insights Cloak

### Static analysis of columns

From version `18.3.0` onwards, Insights Cloak performs an analysis of the columns in a data source when booting.
The analysis determines which columns are likely to isolate users.
Such columns have mostly user-specific values, and therefore behave much like the user-id column. Consider columns containing email addresses or social security numbers as examples.
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
