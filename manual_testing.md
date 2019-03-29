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
