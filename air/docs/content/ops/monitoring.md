# Monitoring

The air component exposes a monitoring endpoint on port 8081. If you want to use it you will have to forward that port
when starting your docker container (as explained in the installation guide). You will also need to generate
a monitoring token for an admin user by going to `/api_tokens` in the web application. Make sure to choose "Monitoring"
from the "Access" dropdown.

After that the endpoint can be accessed at `http://AirWeb.Endpoint:8081/?auth_token=<your monitoring token>`.

## Data format

The endpoint presents data as the following JSON structure:

<pre class="inlined">
  <code>
{
  "version": "the air version",
  "uptime": "time in seconds since the air component started",
  "groups": ["name of group existing in the system"],

  "users": [
    {
      "name": "name of the user",
      "login": "login of the user",
      "groups": ["name of group the user belongs to"]
    },
    ...
  ],

  "cloaks": [
    {
      "name": "name of the cloak",
      "version": "the cloak version",
      "uptime": time in seconds since this cloak connected to the air component,
      "data_sources": [id of data source the cloak provides access to],
      "queries": {
        "last_5_minutes": number of queries made to this cloak in the last 5 minutes,
        "last_15_minutes": ...,
        "last_30_minutes": ...,
        "last_1_hour": ...,
        "last_1_day": ...
      },
      "memory": {
        "total_memory": number of bytes,
        "free_memory": {
          "current": number of free bytes,
          "last_5_seconds": number of free bytes,
          "last_1_minute": number of free bytes,
          "last_5_minutes": number of free bytes,
          "last_15_minutes": number of free bytes,
          "last_1_hour": number of free bytes
        }
      }
    },
    ...
  ],

  "data_sources": [
    {
      "id": id of the data source,
      "name": "name of the data source",
      "groups": ["group with access to the data source"],
      "errors": ["error encountered during data source initialization"],
      "queries": {
        "last_5_minutes": number of queries made to this data source in the last 5 minutes,
        "last_15_minutes": ...,
        "last_30_minutes": ...,
        "last_1_hour": ...,
        "last_1_day": ...
      }
    }
  ]
}
  </code>
</pre>

Please note that the memory statistics given for the cloaks are the minimum values seen within the
given time period. Furthermore all the measurements are in bytes.
