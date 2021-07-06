# Manual tests

The tests should be performed from top to bottom as there are some order dependencies.
For example tables created in the UX are useful for later tests.

## Tests

### Upgrade

- [ ] Reset your `air` system (see [Deploying a clean system](#deploying-a-clean-system) for details)
- [ ] Deploy the previous release of `air` and `cloak`
- [ ] Create analyst views and an analyst table described in [Useful analyst tables and views](#useful-analyst-tables-and-views)
- [ ] Run the queries listed in [Sample queries](#sample-queries)
- [ ] Create an API token and keep it for later tests
- [ ] Deploy the latest release of both `air` and `cloak`
- [ ] Log in
- [ ] Verify that the query history is preserved
- [ ] Verify that the audit log is preserved
- [ ] Verify that the views and the analyst table can be queried
- [ ] Verify that the API token and app login can be used by running the `curl` and `psql` commands from the
      [Insights Air Interface](#insights-air-interface) tests.

### Clean deploy

- [ ] Reset your `air` system (see [Deploying a clean system](#deploying-a-clean-system) for details)
- [ ] Deploy the latest release of `air` and `cloak`
- [ ] Perform the onboarding procedure, as explained in [Deploying a clean system](#deploying-a-clean-system)
- [ ] Perform the [Insights Air Interface](#insights-air-interface) tests.
- [ ] Validate that the version number is right
- [ ] Validate that the changelog has been updated
- [ ] Run the first few queries listed in [Sample queries](#sample-queries) with `EXPLAIN` and validate that the
      query plan yields expected results.
- Validate restrictions:
  - [ ] The following query should fail due to `opponent` being
        excluded (error should say the column doesn't exist):
    ```sql
    SELECT count(distinct opponent) FROM games
    ```
  - [ ] The following query should fail due to `lastname` being
        marked as unselectable:
    ```sql
    SELECT lastname, count(*)
    FROM players
    GROUP BY 1
    ```
  - [ ] The range in the following query should be adjusted to 30 to 50
    ```sql
    SELECT avg(level)
    FROM players
    WHERE age BETWEEN 33 and 43
    ```

### Insights Air Interface

- [ ] Sign out
- [ ] Sign in with your administrator username and credential
- Go to `Data Sources -> GamesAndPlayers`
- [ ] Create analyst views and an analyst table described in [Useful analyst tables and views](#useful-analyst-tables-and-views)
- [ ] Run the queries listed in [Sample queries](#sample-queries)
- [ ] Ensure syntax is highlighted in the editor and in result boxes
- Go to `Cog icon -> API tokens`
- [ ] Create an API token, note it down
- [ ] Issue this curl, setting `$token` to your own API token, and `$url` to the URL of your Aircloak instance

  ```bash
  token=your_own_token
  url=your_aircloak_instance_url

  curl -X POST -H "content-type: application/json" -H "auth-token: $token" \
    -d '{"query": {"statement": "SELECT COUNT(*) FROM games", "data_source_name": "GamesAndPlayers"}}' \
    $url/api/queries
  ```

  - [ ] You will get a query id in response. Issue this command, setting `$query_id` to the id you got:

    ```bash
    query_id=your_own_id

    curl -X GET -H "content-type: application/json" -H "auth-token: $token" \
      $url/api/queries/$query_id | \
      python -m json.tool
    ```

- [ ] Issue the query command with a broken token to validate it fails:

  ```
  token=broken
  # url is already set

  curl -X POST -H "content-type: application/json" -H "auth-token: $token" \
    -d '{"query": {"statement": "SELECT COUNT(*) FROM games", "data_source_name": "GamesAndPlayers"}}' \
    $url/api/queries

  ```

- Go to `Cog icon -> App logins`
- [ ] Create a new login
- [ ] Connect to psql, substituting the created login for `$login`, and `$hostname` with the URL of your Aircloak
      instance and `$port` with the port number your instance is exposed under:
  ```bash
  port=port_number
  hostname=hostname_of_your_aircloak_instance
  login=login_you_created
  psql -h $hostname -p $port -U $login -d GamesAndPlayers
  ```
- [ ] Supply an incorrect password and validate that authentication fails
- Supply the created password
- [ ] Check that `\dt` shows a list of tables, including analyst tables and views
- [ ] Run this query:
  ```sql
  SELECT COUNT(*) FROM anonymizing_view;
  ```
- [ ] Add a non-admin user account with username `user@aircloak.com` and password `password1234` and give the user access to the data source
- Sign out
- [ ] Sign in as the non-admin user and repeat the steps above, namely:
  - [ ] Creating views and analyst tables
  - [ ] Running queries in the web interface
  - [ ] Creating API token and issuing a query
  - [ ] Creating App login and querying using `psql`

### Sample queries

- [ ] Issue the following queries. They should all return non-empty results:
  - ```sql
    SELECT count(*) FROM num_games_view
    ```
  - ```sql
    SELECT count(*) FROM num_games_table
    ```
  - ```sql
    SELECT * FROM anonymizing_view
    ```
  - ```sql
    SELECT numGames, count(*)
    FROM num_games_table
    GROUP BY 1
    ORDER BY numGames ASC
    ```
  - [ ] Click on `Show chart`, select `numGames` as `X` and `count` as `Y`, make sure the chart displays
  - ```sql
    SELECT
      age,
      avg(
        CASE
        WHEN firstname = 'Reese' THEN 1
        WHEN firstname = 'Kendall' THEN 1
        WHEN firstname = 'Ellie' THEN 1
        ELSE 0
        END
      )
    FROM players
    GROUP BY age
    ORDER BY 1 ASC
    ```
  - ```sql
    SELECT gender, level, age
    FROM players
    ```

### Tableau tests

- Add a new Postgres datasource in Tableau connecting to the database `GamesAndPlayers` on your Aircloak instance using the app login created in the Insights Air interface tests
  - [ ] Configure it to not use SSL and validate that it connects
  - [ ] Click "Edit connection" and edit it to require SSL. Validate that it connects
- Configure tables
  - [ ] Drag the following tables (in the described order) into the tables canvas: `players`, `games`
  - [ ] Configure the join conditions: `players.uid = games.player`
- [ ] Ensure the connection type is set to `Live` top right
- [ ] Rename the data source to "Test datasource"
- [ ] Click on "Sheet 1" bottom left to create a new analysis sheet
- [ ] Right-click on "Age" and choose that it should be interpreted
      as a dimension.
- [ ] Drag "Age" to the rows area
- [ ] Drag "Players (count)" to the column next to the age values in the middle of the screen
- You should now see a table of ages and their counts
- [ ] Click "show me" and turn it into a bar graph
- [ ] Click the icon to toggle rows and columns to make it horizontal
- [ ] Drag "players.Gender" onto the rows area
- You should now see a bar graph of num games per gender and age
- [ ] Drag "games.Date" to the filters section, then select months, and subsequently April and May.
- The graph should still display sensible values
- [ ] Remove the "age" column
- [ ] Drag "games.Date" to the columns area
- You should see the num games per player per year now
- [ ] Click the + icon in the "YEAR(Date)" pill in the columns area twice to expand it to quarter and month
- It should now render a new graph, and everything should work as expected

## How to's

### Deploying a clean system

The system can be deployed to the developer's private `air`/`cloak` combination which is configured in the `./deployed_targets` folder.

To deploy a clean version of the system, you can perform the following steps.

1. Ensure the `air` config allows for, but does not require SSL connections (`psql_server/require_ssl = false`)
1. Stop the `cloak` (e.g. `docker stop sebastian_cloak`)
1. Start remote iex session to the running `air` instance (e.g. `docker exec -it sebastian_air /aircloak/air/bin/air remote_console`)
1. Reset the database to the initial state by invoking `Ecto.Migrator.run(Air.Repo, Application.app_dir(:air, "priv/repo/migrations"), :down, all: true)`
1. Exit the shell and stop the `air` instance (e.g. `docker stop sebastian_air`)
1. Checkout the desired branch (typically the release branch), and deploy `air` first, and then `cloak`

At this point, you have a clean installation of `air`.

Now, you need to perform the onboarding procedure and create the first user. Visit your `air` site (e.g. https://sebastian-air.aircloak.com), and follow the instructions.

Finally, you need to provide permissions for each desired data source. In your `air`, go to Settings/Admin/Groups, click on the Edit button next to the admin group, add all available data sources, and click on save.

At this point, you can query data sources from the main user interface.

### Data sources definition

The `GamesAndPlayers` is used for testing.
The data source definition is:

```json
{
  "driver": "postgresql",
  "name": "GamesAndPlayers",
  "lcf_buckets_aggregation_limit": 0,
  "analyst_tables_enabled": true,
  "parameters": {
    "hostname": "airdb.mpi-sws.org",
    "username": "newcloak",
    "database": "newcloak",
    "password": "yWNa7-M8NyF(Y~y}qn6XzRHo"
  },
  "tables": {
    "players": {
      "keys": [{ "user_id": "uid" }],
      "unselectable_columns": ["lastname"]
    },
    "games": {
      "keys": [{ "user_id": "player" }],
      "exclude_columns": ["opponent"]
    }
  }
}
```

Make sure your Aircloak instance has this data source.

### Useful analyst tables and views

Use the following SQL to create:

- an analyst table named: `num_games_table`
- an analyst view named: `num_games_view`

```SQL
SELECT player, count(*) as numGames
FROM games
GROUP BY player
```

Now create an anonymized analyst view named `anonymizing_view` with the following SQL:

```SQL
SELECT age, count(*)
FROM players
GROUP BY age
ORDER BY age ASC
```
