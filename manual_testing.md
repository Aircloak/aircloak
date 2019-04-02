# Clean deploy

- Reset your air system (see [Deploying a clean system](#deploying-a-clean-system) for details)
- Deploy the latest release of air and cloak
- [ ] Perform the onboarding procedure, as explained in [Deploying a clean system](#deploying-a-clean-system)
- [ ] Proceed with [Insights Air Interface](#insights-air-interface) tests

# Upgrade

- Reset your air system (see [Deploying a clean system](#deploying-a-clean-system) for details)
- Deploy the previous release of air and cloak
- Create one view and one analyst table
- Issue a couple of queries using plain tables, the view, and the analyst table
- Sign out
- Deploy the latest release of both air and cloak
- [ ] Log in
- [ ] Verify that the query history is preserved
- [ ] Verify that the view and the analyst table can be used from queries
- [ ] Proceed with [Insights Air Interface](#insights-air-interface) tests

# Insights Air Interface

- Start air and cloak in dev
- [ ] Sign out
- [ ] Sign in (admin@aircloak.com/password1234)
- Go to `Data Sources -> cloak_postgres_native`
- [ ] Issue this query:
  ```sql
  SELECT itemname, COUNT(*)
  FROM purchases
  GROUP BY 1
  ```
  - [ ] Ensure syntax is highlighted in the editor and in result boxes
- [ ] Click on `Show chart`, select `itemname` as `X` and `count` as `Y`, make sure the chart displays
- Go to `Cog icon -> API tokens`
- [ ] Create an API token, note it down
- [ ] Issue this curl, setting `$token` to your own API token
  ```bash
  token=your_own_token

  curl -X POST -H "content-type: application/json" -H "auth-token: $token" \
    -d '{"query": {"statement": "SELECT COUNT(*) FROM purchases", "data_source_name": "cloak_postgres_native"}}' \
    localhost:8080/api/queries
  ```
  - [ ] You will get a query id in response. Issue this command, setting `$query_id` to the id you got:
    ```bash
    query_id=your_own_id

    curl -X GET -H "content-type: application/json" -H "auth-token: $token" \
      localhost:8080/api/queries/$query_id \
      python -m json.tool
    ```
- Go to `Cog icon -> App logins`
- [ ] Create a new login
- [ ] Connect to psql, substituting the created login for `$login`:
  ```bash
  psql -h localhost -p 8432 -U $login cloak_postgres_native
  ```
- Supply the created password
- [ ] Check that `\dt` shows a list of tables
- [ ] Run this query:
  ```sql
  SELECT COUNT(*) FROM purchases;
  ```

# Deploying a clean system

The system can be deployed to the developer's private air/cloak combination which is configured in the `./deployed_targets` folder.

To deploy a clean version of the system, you can perform the following steps.

1. Stop the cloak (e.g. `docker stop sasa_cloak`)
2. Start remote iex session to the running air instance (e.g. `docker exec -it sasa_air /aircloak/air/bin/air remote_console`)
3. Reset the database to the initial state by invoking `Ecto.Migrator.run(Air.Repo, Application.app_dir(:air, "priv/repo/migrations"), :down, all: true)`
4. Exit the shell and stop the air instance (e.g. `docker stop sasa_air`)
5. Checkout the desired branch (typically the release branch), and deploy air first, and then cloak

At this point, you have the clean installation of air.

Now, you need to perform the onboarding procedure and create the first user. Visit your air site (e.g. https://sasa-air.aircloak.com), and follow the instructions.

Next, you need to import the license. To obtain the license, you can visit https://central.aircloak.com/, click the "Licenses" button next to the Aircloak customer, find your user, and export the license to your disk. Then you can visit your air, go to Settings/Admin/Aircloak license, and import it.

Finally, you need to provide permissions for each desired data source. In your air, go to Settings/Admin/Data sources, click on the Edit button next to the desired data source, check the admin group, and click on save.

At this point, you can query data sources from the main user interface.
