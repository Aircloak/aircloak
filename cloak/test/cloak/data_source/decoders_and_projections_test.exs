defmodule Cloak.DataSource.DecodersAndProjectionsTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.{Table, MongoDB}

  import Cloak.Test.MongoHelpers

  @moduletag :exclude_in_dev
  @moduletag :mongodb

  setup_all do
    parameters = [hostname: "localhost", database: "cloaktest"]
    {:ok, conn} = Mongo.start_link(parameters)

    Mongo.delete_many(conn, "dec_proj_enc_base", %{})
    Mongo.delete_many(conn, "dec_proj_projected_base", %{})

    for i <- 1..10 do
      Mongo.insert_one!(conn, "dec_proj_enc_base", %{pk: i, val: "10"})
      Mongo.insert_one!(conn, "dec_proj_projected_base", %{fk: i})
    end

    tables = %{
      dec_proj_enc: %{
        user_id: "_id",
        db_name: "dec_proj_enc_base",
        decoders: [
          %{method: "text_to_integer", columns: ["val"]}
        ]
      },
      dec_proj_projected: %{
        user_id: "uid",
        db_name: "dec_proj_projected_base",
        projection: %{
          table: "dec_proj_enc",
          primary_key: "pk",
          foreign_key: "fk",
          user_id_alias: "uid"
        }
      }
    }

    data_source =
      Table.load(
        %{
          name: "mongo_db_decoders_and_projections",
          concurrency: 0,
          driver: MongoDB,
          parameters: parameters,
          tables: tables,
          errors: [],
          key: [],
          driver_info: "3.4.0"
        },
        conn
      )

    GenServer.stop(conn)
    {:ok, data_source: data_source}
  end

  test "show columns from an encoded table", context do
    assert_query(context, "show columns from dec_proj_enc", %{
      rows: [
        %{row: ["_id", "text"]},
        %{row: ["val", "integer"]},
        %{row: ["pk", "real"]}
      ]
    })
  end

  test "show columns from a projected table", context do
    assert_query(context, "show columns from dec_proj_projected", %{
      rows: [
        %{row: ["uid", "text"]},
        %{row: ["_id", "text"]},
        %{row: ["fk", "real"]}
      ]
    })
  end

  test "query on encoded table", context do
    assert_query(context, "SELECT val FROM dec_proj_enc WHERE val = 10", %{
      rows: [%{occurrences: 10, row: [10]}]
    })
  end

  test "query on projected table", context do
    assert_query(context, "SELECT count(*) FROM dec_proj_projected", %{
      rows: [%{occurrences: 1, row: [10]}]
    })
  end

  test "query with join between decoded and projected tables", context do
    assert_query(
      context,
      """
        SELECT count(distinct t1.uid)
        FROM dec_proj_projected AS t1 JOIN dec_proj_enc AS t2
        ON t1.uid = t2._id AND t2.val = 10
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end
end
