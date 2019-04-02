# Test preparations

## Prerequisites

You should have the following:
- a local Aircloak instance

## Preparation

1. Configure your air instance to allow, but not require, SSL connectivity on the Postgres port
2. Ensure you have the `cloak_postgres_native` data source available (if not run `make recreate-db-more-data` in the `cloak` directory)
3. Create an analyst table (SQL below)
4. Create two views (one as an anonymizing query and one that's not. SQL below)
5. Validate that you can query all created tables and views

### Table creations

Create an analyst table and analyst view named `per_user_purchases_tab` and `per_user_purchases_view` respectively,
using the following SQL query:

```SQL
SELECT customer_id, count(t.id) as numPurchases
FROM transactions t INNER JOIN accounts a ON a.id = t.account_id
GROUP BY customer_id
```

Validate that you can query them using the queries such as:

```SQL
SELECT numPurchases, count(*)
FROM per_user_purchases_view
GROUP BY 1
ORDER BY numPurchases ASC
```

Now create an anonymized analyst view named `anonymizing_view` with the following SQL:

```SQL
SELECT bucket(amount by 100 align middle) as amount_bucket, count(*)
FROM transactions t INNER JOIN accounts a ON a.id = t.account_id
GROUP BY amount_bucket
ORDER BY amount_bucket ASC
```

and validate it by running the following query:

```SQL
SELECT * FROM anonymizing_view
```
