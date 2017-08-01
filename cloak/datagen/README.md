# Cloak test data generation

This folder contains scripts for setting up your local `cloak` database.
It should be invoked with `make recreate-db` from your main `cloak`-folder.

There is a second version available that generates a larger dataset.
It can be invoked with `make recreate-db-more-data`.


## Compliance test data

Additionally to the above mentioned dataset, a dataset is created that is used
for the compliance integration test. This dataset is imported into all our supported
datasources, in order to ensure that they all yield the same results for the same queries.
This dataset is also added to your local `cloak`.

With the exception of MongoDB, we manually need to create the tables to hold the compliance
test data. The commands to do so are as follows:

### POSTGRESQL

```
CREATE TABLE users (id integer, name text, age integer, active boolean, height real);
CREATE TABLE users_encoded (id integer, name text, age text, active text, height text);
CREATE TABLE notes (id integer, user_id integer, title text, content text);
CREATE TABLE notes_encoded (id integer, user_id integer, title text, content text);
CREATE TABLE drafts_changes (id integer, note_id integer, "changes.date" timestamp, "changes.change" text);
CREATE TABLE drafts_changes_encoded (id integer, note_id integer, "changes.date" text, "changes.change" text);
CREATE TABLE addresses (id integer, user_id integer, "home.city" text, "home.postal_code" integer, "work.city" text,
"work.postal_code" integer);
CREATE TABLE addresses_encoded (id integer, user_id integer, "home.city" text, "home.postal_code" text, "work.city"
text, "work.postal_code" text);
```

### MySQL

```
CREATE TABLE users (id integer, name text, age integer, active boolean, height real);
CREATE TABLE users_encoded (id integer, name text, age text, active text, height text);
CREATE TABLE notes (id integer, user_id integer, title text, content text);
CREATE TABLE notes_encoded (id integer, user_id integer, title text, content text);
CREATE TABLE drafts_changes (id integer, note_id integer, `changes.date` timestamp, `changes.change` text);
CREATE TABLE drafts_changes_encoded (id integer, note_id integer, `changes.date` text, `changes.change` text);
CREATE TABLE addresses (id integer, user_id integer, `home.city` text, `home.postal_code` integer, `work.city` text,
`work.postal_code` integer);
CREATE TABLE addresses_encoded (id integer, user_id integer, `home.city` text, `home.postal_code` text, `work.city`
text, `work.postal_code` text);
```

### MSSQL

```
CREATE TABLE users (id integer, name text, age integer, active bit, height real);
CREATE TABLE users_encoded (id integer, name text, age text, active text, height text);
CREATE TABLE notes (id integer, user_id integer, title text, content text);
CREATE TABLE notes_encoded (id integer, user_id integer, title text, content text);
CREATE TABLE drafts_changes (id integer, note_id integer, "changes.date" timestamp, "changes.change" text);
CREATE TABLE drafts_changes_encoded (id integer, note_id integer, "changes.date" text, "changes.change" text);
CREATE TABLE addresses (id integer, user_id integer, "home.city" text, "home.postal_code" integer, "work.city" text,
"work.postal_code" integer);
CREATE TABLE addresses_encoded (id integer, user_id integer, "home.city" text, "home.postal_code" text, "work.city"
text, "work.postal_code" text);
```
