# MongoCleanup

Tool for applying decoders and projections ahead of time in a mongo database.

The tool takes a cloak config file with decoders and projections defined. It connects to the database specified and
modifies all documents that require decoding or projection. It applies the specified decoders and adds a `_ac_user_id`
field to all documents that require projection. Finally, it **destructively** updates the documents in the source
database.

# Development

## Starting mongo

If you have mongo installed natively you may do something like:

```
mongod --dbpath /tmp
```

Otherwise you can use the provided make task to start a mongo server in a docker container:

```
make mongo-server
```

## Running

You can use

```
make run
```

to perform a sanity-check run on the Teambank example data included in the project directory. The task will regenerate
the database from the included dump file (`teambank_dump.gz`) and run the tool with the appropriate config
(`teambank_mongo_34.json`). You can also just run the db regeneration part with:

```
make reset-db
```

To perform a custom run use:

```
dotnet run --project MongoCleanup
```

There is also a data generator included to perform rough performance measurements, you can run it using:

```
SIZE=100 make run-perf
```

The `SIZE` variable controls the number of users to generate. Note that each user has ~10 purchases, each with around
~10 items, so the final number of records will be about 100 times larger than what you specify here.

## Running tests

Use

```
dotnet test
```

to run all tests.

## Releasing

Use

```
make publish
```

to generate releases for both linux and OSX. The releases are created in
`MongoCleanup/bin/Release/netcoreapp2.1/{linux,osx}-x64/publish/` - this whole directory needs to be copied, but it's
a standalone app, not requiring the dotnet runtime to be installed.
