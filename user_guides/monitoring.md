# Monitoring

The air component exposes a monitoring endpoint on port 8081. If you want to use it you will have to forward that port
when starting your docker container (see user_guides/installation.md). You will also need to generate an authentication
token for an admin user by going to `/api_tokens` in the web application.

After that the endpoint can be accessed at `air.endpoint:8081/?auth_token=<your auth token>`.

## Data format

The endpoint presents data as the following JSON structure:

```json
{
  uptime: <time in seconds since the air component started>,
  groups: <list of group names>,

  users: [
    {
      name: <name of the user>,
      email: <email of the user>,
      groups: <list of the groups the user belongs to>
    },
    ...
  ],

  cloaks: [
    {
      name: <name of the cloak>,
      uptime: <time in seconds since this cloak connected to the air component>,
      data_sources: <list of ids of data sources the cloak provides access to>,
      queries: {
        last_5_minutes: <number of queries made to this cloak in the last 5 minutes>,
        last_15_minutes: ...,
        last_30_minutes: ...,
        last_1_hour: ...,
        last_1_day: ...
      }
    },
    ...
  ],

  data_sources: [
    {
      id: <id of the data source>,
      name: <name of the data source>,
      groups: <list of groups with access to the data source>,
      queries: {
        last_5_minutes: <number of queries made to this data source in the last 5 minutes>,
        last_15_minutes: ...,
        last_30_minutes: ...,
        last_1_hour: ...,
        last_1_day: ...
      }
    }
  ]
}
```
