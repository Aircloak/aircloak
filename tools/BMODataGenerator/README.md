# BMO test data generator

Tool for generating test data in order to have data that can be used for
producing a large dataset for performance test for BMO.

Since the goal was to produce parquet files that can be consumed by Drill,
I opted for generating JSON data from a data generator which I then convert
to parquet using Drill. That way I am positive Drill can also later consume
the parquet files.

It is a simplistic generator, and I only understood the ramification of using
`Async.Parallel` after the fact (namely that it consumes the whole sequence
immediately. This means that there is a somewhat high start time when generating
large amounts of user data which is not parallelized.

I also think the process could be simplified by not parallelizing the system
that much in the first place. However some simple performance tests seemed to
indicate that the current version generates data with an _adequate_ (not great) speed.

The goal was to create a dataset with some 100 million users. At the time of writing
I have used the tool to generate data for some 10 million users (roughly 250 million rows).

## Converting to parquet

### Start drill

To start a drillbit:

```
docker \
  run -i \
  -v FOLDER_WITH_DATA:/opt/generated \
  -p 8047:8047 \
  -p 31010:31010 \
  -t \
  drill/apache-drill:1.15.0 /bin/bash
```

Once the drillbit is running you need to adjust the configuration so you have
a writeable space. The config could look something like this:

```json
{
  "type": "file",
  "connection": "file:///",
  "config": null,
  "workspaces": {
    "generated": {
      "location": "/opt/generated/output",
      "writable": true,
      "defaultInputFormat": null,
      "allowAccessOutsideWorkspace": false
    },
    ...
  },
  ...
}
```

And then you can create the parquet file using

```sql
CREATE TABLE dfs.generated.genesis_parquet AS (
  SELECT
    cast(user_id as int) as user_id,
    cast(merch_name as varchar) as merch_name,
    cast(merch_sic as varchar) as merch_sic,
    cast(merch_city as varchar) as merch_city,
    cast(merch_state as varchar) as merch_state,
    cast(tran_amt as double) as tran_amt,
    cast(tran_dt as date) as tran_dt,
    cast(spouse as varchar) as spouse,
    cast(work_place as varchar) as work_place,
    cast(annual_inc as varchar) as annual_inc,
    cast(card_nbr as varchar) as card_nbr,
    cast(genesis_id as int) as genesis_id
  FROM dfs.`/opt/generated/data.json`
);
```

## Building release


You can build a standalone version for OSX and Linux as follows:

- macOS: `make publish-osx`
- linux: `make publish-linux`
- both: `make publish`

## Running

`make start` will run the program and save data for 10 users in a file called `test.json`.
