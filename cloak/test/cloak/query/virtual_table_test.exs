defmodule Cloak.Query.VirtualTableTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  import ExUnit.CaptureLog

  setup_all do
    # standard tests
    :ok = Cloak.Test.DB.create_table("vtt_real", "ival INTEGER")
    :ok = insert_rows(_user_ids = 1..10, "vtt_real", ["ival"], [10])
    :ok = insert_rows(_user_ids = 1..10, "vtt_real", ["ival"], [20])

    # encoded columns tests
    :ok = Cloak.Test.DB.create_table("vtt_encoded", "int_val TEXT, real_val TEXT, bool_val TEXT")

    # projected tests
    Cloak.Test.DB.create_table("projected_accounts", "id INTEGER, name TEXT")
    Cloak.Test.DB.create_table("projected_heights", "height integer")

    Cloak.Test.DB.create_table(
      "projected_transactions_real",
      "id INTEGER, account_id INTEGER, amount INTEGER",
      add_user_id: false
    )

    Cloak.Test.DB.create_table(
      "projected_notes_real",
      "transaction_id INTEGER, note TEXT",
      add_user_id: false
    )

    Enum.each(
      1..20,
      &insert_rows(&1..&1, "projected_accounts", ["id", "name"], [&1, "account_#{&1}"])
    )

    Enum.each(
      1..10,
      &insert_rows(&1..&1, "projected_accounts", ["id", "name"], [20 + &1, "account_#{20 + &1}"])
    )

    Enum.each(1..30, &insert_transaction(&1, 100, "note text"))
    insert_rows(_user_ids = 1..100, "projected_heights", ["height"], [180])

    :ok
  end

  defp create_virtual_table!(name, statement, opts \\ []) do
    :ok = Cloak.Test.DB.create_table(name, nil, [skip_db_create: true, query: statement] ++ opts)
    on_exit(fn -> Cloak.Test.DB.delete_table(name) end)
    :ok
  end

  defp insert_transaction(id, amount, note) do
    Cloak.Test.DB.insert_data("projected_transactions_real", ["id", "account_id", "amount"], [
      [id, id, amount]
    ])

    Cloak.Test.DB.insert_data("projected_notes_real", ["transaction_id", "note"], [
      [id, "(1) " <> note]
    ])

    Cloak.Test.DB.insert_data("projected_notes_real", ["transaction_id", "note"], [
      [id, "(2) " <> note]
    ])
  end

  defp virtual_table_error(query), do: capture_log(fn -> create_virtual_table!("vtt_fake", query) end)

  test "error on query parsing" do
    assert virtual_table_error("_select * from vtt_real") =~ ~r/Failed to parse the query for virtual table `vtt_fake`/
  end

  test "error on query compilation" do
    assert virtual_table_error("select x from cloak_test.vtt_real") =~
             ~r/Failed to compile the query for virtual table `vtt_fake`/
  end

  test "error on invalid column name" do
    assert virtual_table_error("select user_id, ival * 2 from cloak_test.vtt_real") =~
             ~r/Invalid column name `\*` in virtual table `vtt_fake`/
  end

  test "error on invalid query" do
    assert virtual_table_error("select user_id, xxx(ival) as x from cloak_test.vtt_real") =~ ~r/Unknown function `xxx`/
  end

  test "duplicated columns are dropped" do
    create_virtual_table!("vtt_fake", "select user_id, * from cloak_test.vtt_real")

    assert_query("show columns from vtt_fake", %{
      rows: [
        %{row: ["user_id", "text"]},
        %{row: ["ival", "integer"]}
      ]
    })
  end

  test "constant columns are dropped" do
    create_virtual_table!("vtt_fake", "select *, 0 as a from cloak_test.vtt_real")

    assert_query("show columns from vtt_fake", %{
      rows: [
        %{row: ["user_id", "text"]},
        %{row: ["ival", "integer"]}
      ]
    })
  end

  test "hiding columns by dropping duplicated and constant columns" do
    create_virtual_table!("vtt_fake", "select 0 as ival, * from cloak_test.vtt_real")

    assert_query("show columns from vtt_fake", %{
      rows: [
        %{row: ["user_id", "text"]}
      ]
    })
  end

  test "virtual columns" do
    create_virtual_table!("vtt_fake", "select user_id, ival * 2 as x from cloak_test.vtt_real")
    assert_query("select sum(x) from vtt_fake", %{rows: [%{row: [600]}]})
  end

  test "filtered virtual table" do
    create_virtual_table!("vtt_fake", "select * from cloak_test.vtt_real where ival = 10")
    assert_query("select sum(ival) from vtt_fake", %{rows: [%{row: [100]}]})
  end

  test "virtual table with subquery" do
    create_virtual_table!(
      "vtt_fake",
      "select * from (select user_id, ival from cloak_test.vtt_real) as t"
    )

    assert_query("select sum(ival) from vtt_fake", %{rows: [%{row: [300]}]})
  end

  test "virtual table from joined tables" do
    create_virtual_table!("vtt_fake", """
      select t1.user_id, t1.ival as i1, t2.ival as i2
      from cloak_test.vtt_real as t1 join cloak_test.vtt_real as t2
      on t1.user_id = t2.user_id and t1.ival <> t2.ival
    """)

    assert_query("select sum(i1+i2) from vtt_fake", %{rows: [%{row: [600]}]})
  end

  describe "data decoding tests" do
    setup do
      create_virtual_table!("vtt_decoded", """
        select
          user_id, cast(int_val as integer), cast(real_val as real), cast(bool_val as boolean)
        from cloak_test.vtt_encoded
      """)

      Cloak.Test.DB.clear_table("vtt_encoded")
      :ok
    end

    test "simple decoding of encoded booleans" do
      :ok = insert_rows(_user_ids = 1..10, "vtt_encoded", ["bool_val"], ["true"])
      :ok = insert_rows(_user_ids = 11..20, "vtt_encoded", ["bool_val"], ["TRUE"])
      :ok = insert_rows(_user_ids = 21..30, "vtt_encoded", ["bool_val"], ["False"])
      :ok = insert_rows(_user_ids = 31..40, "vtt_encoded", ["bool_val"], [nil])

      assert_query("select bool_val from vtt_decoded where bool_val is not null order by bool_val", %{
        rows: [%{occurrences: 10, row: [false]}, %{occurrences: 20, row: [true]}]
      })

      assert_query("select bool_val from vtt_decoded where bool_val = true", %{
        rows: [%{occurrences: 20, row: [true]}]
      })

      assert_query("select bool_val from vtt_decoded where bool_val <> true", %{
        rows: [%{occurrences: 10, row: [false]}]
      })
    end

    test "simple decoding of encoded reals" do
      :ok = insert_rows(_user_ids = 1..10, "vtt_encoded", ["real_val"], ["1.22"])
      :ok = insert_rows(_user_ids = 11..20, "vtt_encoded", ["real_val"], ["22.0"])
      :ok = insert_rows(_user_ids = 21..30, "vtt_encoded", ["real_val"], [nil])

      assert_query("select real_val from vtt_decoded where real_val is not null order by real_val", %{
        rows: [%{occurrences: 10, row: [1.22]}, %{occurrences: 10, row: [22.0]}]
      })

      assert_query("select real_val from vtt_decoded where real_val = 1.22", %{
        rows: [%{occurrences: 10, row: [1.22]}]
      })

      assert_query("select real_val from vtt_decoded where real_val <> 1.22", %{
        rows: [%{occurrences: 10, row: [22.0]}]
      })
    end

    test "simple decoding of encoded integers" do
      :ok = insert_rows(_user_ids = 1..10, "vtt_encoded", ["int_val"], ["111"])
      :ok = insert_rows(_user_ids = 11..20, "vtt_encoded", ["int_val"], ["222"])
      :ok = insert_rows(_user_ids = 21..30, "vtt_encoded", ["int_val"], [nil])

      assert_query("select int_val from vtt_decoded where int_val is not null order by int_val", %{
        rows: [%{occurrences: 10, row: [111]}, %{occurrences: 10, row: [222]}]
      })

      assert_query("select int_val from vtt_decoded where int_val = 111", %{
        rows: [%{occurrences: 10, row: [111]}]
      })

      assert_query("select int_val from vtt_decoded where int_val <> 222", %{
        rows: [%{occurrences: 10, row: [111]}]
      })
    end

    test "decoding of encoded data from a join" do
      :ok = Cloak.Test.DB.create_table("vtt_encoded_join", "_dummy INTEGER")
      on_exit(fn -> Cloak.Test.DB.delete_table("vtt_encoded_join") end)
      :ok = insert_rows(_user_ids = 1..15, "vtt_encoded", ["int_val"], ["123"])
      :ok = insert_rows(_user_ids = 10..20, "vtt_encoded_join", [], [])

      assert_query(
        """
          select int_val from
          vtt_decoded as t1 left join vtt_encoded_join as t2 on t1.user_id = t2.user_id
          where t2.user_id is null
        """,
        %{rows: [%{occurrences: 9, row: [123]}]}
      )
    end
  end

  test "[Issue #1538] joins with virtual tables are order-independent" do
    :ok = Cloak.Test.DB.create_table("vtt_join1", "row_id INTEGER")

    :ok =
      Cloak.Test.DB.create_table(
        "vtt_join_real1",
        "row_id INTEGER, one_id INTEGER",
        add_user_id: false
      )

    :ok =
      Cloak.Test.DB.create_table(
        "vtt_join_real2",
        "one_id INTEGER, two_id INTEGER",
        add_user_id: false
      )

    create_virtual_table!("vtt_join2", """
      select user_id, jr1.row_id, jr1.one_id from
      cloak_test.vtt_join_real1 as jr1 join cloak_test.vtt_join1 as j1 on jr1.row_id = j1.row_id
    """)

    create_virtual_table!("vtt_join3", """
      select user_id, jr2.one_id, jr2.two_id from
      cloak_test.vtt_join_real2 as jr2 join cloak_test.vtt_join_real1 as jr1 on jr2.one_id = jr1.one_id
      join cloak_test.vtt_join1 as j1 on jr1.row_id = j1.row_id
    """)

    :ok = insert_rows(_user_ids = 1..20, "vtt_join1", ["row_id"], [100])
    data = 1..20 |> Enum.to_list() |> Enum.map(fn _ -> [100, 100] end)
    Cloak.Test.DB.insert_data("vtt_join_real1", ["row_id", "one_id"], data)
    Cloak.Test.DB.insert_data("vtt_join_real2", ["one_id", "two_id"], data)

    assert_query(
      """
        select count(*) from vtt_join3 inner join vtt_join2
          on vtt_join3.user_id = vtt_join2.user_id
          and vtt_join2.row_id = vtt_join3.two_id
          inner join vtt_join1 as vtt_join1
          on vtt_join1.user_id = vtt_join2.user_id
          and vtt_join1.row_id = vtt_join2.one_id
      """,
      %{rows: [%{row: [count]}]}
    )

    assert_query(
      """
        select count(*) from vtt_join1 inner join vtt_join2
          on vtt_join1.user_id = vtt_join2.user_id
          and vtt_join1.row_id = vtt_join2.one_id
          inner join vtt_join3
          on vtt_join3.user_id = vtt_join2.user_id
          and vtt_join2.row_id = vtt_join3.two_id
      """,
      %{rows: [%{row: [^count]}]}
    )
  end

  describe "table projection tests" do
    setup do
      create_virtual_table!("projected_transactions", """
        select a.user_id, t.* from
        cloak_test.projected_transactions_real as t
        join cloak_test.projected_accounts as a on t.account_id = a.id
      """)

      create_virtual_table!(
        "projected_notes",
        """
          select a.user_id as uid, n.*
          from cloak_test.projected_notes_real as n
          join cloak_test.projected_transactions_real as t on n.transaction_id = t.id
          join cloak_test.projected_accounts as a on t.account_id = a.id
        """,
        user_id: "uid"
      )
    end

    test "projected tables are included in show tables" do
      assert_query("show tables", %{rows: rows})
      assert(Enum.any?(rows, &(hd(&1.row) == "projected_transactions")))
      assert(Enum.any?(rows, &(hd(&1.row) == "projected_notes")))
    end

    test "show columns from a projected table" do
      assert_query("show columns from projected_transactions", %{
        rows: [
          %{row: ["user_id", "text"]},
          %{row: ["id", "integer"]},
          %{row: ["account_id", "integer"]},
          %{row: ["amount", "integer"]}
        ]
      })
    end

    test "show columns from a multiply projected table" do
      assert_query("show columns from projected_notes", %{
        rows: [
          %{row: ["uid", "text"]},
          %{row: ["transaction_id", "integer"]},
          %{row: ["note", "text"]}
        ]
      })
    end

    test "selecting from a projected table" do
      assert_query("select amount from projected_transactions", %{
        columns: ["amount"],
        rows: [%{row: [100], occurrences: 30}]
      })
    end

    test "selecting from an aliased projected table" do
      assert_query("select pt.amount from projected_transactions pt", %{
        columns: ["amount"],
        rows: [%{row: [100], occurrences: 30}]
      })
    end

    test "selecting from a multiply projected table" do
      assert_query("select note from projected_notes order by note", %{
        columns: ["note"],
        rows: [
          %{row: ["(1) note text"], occurrences: 30},
          %{row: ["(2) note text"], occurrences: 30}
        ]
      })
    end

    test "expression in a projected table" do
      assert_query("select abs(amount) from projected_transactions", %{
        columns: ["abs"],
        rows: [%{row: [100], occurrences: 30}]
      })
    end

    test "projected table in a subquery" do
      assert_query("select amount from (select user_id, amount from projected_transactions) sq_alias", %{
        columns: ["amount"],
        rows: [%{row: [100], occurrences: 30}]
      })
    end

    test "expression in a subquery using a projected table" do
      assert_query("select x from (select user_id, abs(amount) as x from projected_transactions) t", %{
        columns: ["x"],
        rows: [%{row: [100], occurrences: 30}]
      })

      assert_query("select x from (select user_id, sqrt(abs(amount)) as x from projected_transactions) t", %{
        columns: ["x"],
        rows: [%{row: [10.0], occurrences: 30}]
      })
    end

    test "joining to a projected table" do
      assert_query("
          select height, amount
          from projected_heights
          inner join projected_transactions on projected_heights.user_id = projected_transactions.user_id
        ", %{columns: ["height", "amount"], rows: [%{row: [180, 100], occurrences: 30}]})

      assert_query("
          select height, amount
          from projected_transactions
          inner join projected_heights on projected_heights.user_id = projected_transactions.user_id
        ", %{columns: ["height", "amount"], rows: [%{row: [180, 100], occurrences: 30}]})
    end
  end
end
