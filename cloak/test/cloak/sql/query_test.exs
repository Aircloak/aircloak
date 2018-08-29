defmodule Cloak.Sql.QueryTest do
  use ExUnit.Case, async: false

  alias Cloak.Sql.{Query, Expression, Parser, Compiler}
  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok =
      Cloak.Test.DB.create_table(
        "feat_describe",
        "id INTEGER, t TEXT, f FLOAT, b BOOLEAN, d TIMESTAMP",
        add_user_id: false,
        user_id: "id"
      )

    :ok = Cloak.Test.DB.create_table("feat_users", "height INTEGER, name TEXT, male BOOLEAN")

    :ok = Cloak.Test.DB.create_table("feat_purchases", "price INTEGER, name TEXT, datetime TIMESTAMP")

    :ok = Cloak.Test.DB.create_table("feat_emulated_users_real", "height TEXT")

    :ok =
      Cloak.Test.DB.create_table(
        "feat_emulated_users",
        nil,
        skip_db_create: true,
        query: """
          SELECT user_id, dec_b64(height) as decoded_height FROM cloak_test.feat_emulated_users_real
        """
      )

    :ok
  end

  describe "top level selected" do
    test "simple case" do
      assert %{num_top_level_dimensions: 1, num_top_level_aggregates: 0} =
               features_from("SELECT height FROM feat_users")

      assert %{num_top_level_dimensions: 3, num_top_level_aggregates: 0} =
               features_from("SELECT height, name, male FROM feat_users")
    end

    test "duplicates" do
      assert %{num_top_level_dimensions: 3, num_top_level_aggregates: 0} =
               features_from("SELECT name, name, name FROM feat_users")
    end

    test "constants" do
      assert %{num_top_level_dimensions: 1, num_top_level_aggregates: 0} = features_from("SELECT '1' FROM feat_users")
    end

    test "aggregates" do
      assert %{num_top_level_dimensions: 1, num_top_level_aggregates: 3} =
               features_from("SELECT name, avg(height), avg(height), sum(height) FROM feat_users GROUP BY name")
    end
  end

  describe "num_db_columns" do
    test "simple query" do
      assert %{num_db_columns: 3, num_distinct_db_columns: 2} = features_from("SELECT height FROM feat_users")
    end

    test "deduplication" do
      assert %{num_db_columns: 4, num_distinct_db_columns: 2} =
               features_from("""
                 SELECT count(*) FROM
                   (SELECT user_id FROM feat_users WHERE height = 0) x
                 INNER JOIN
                   (SELECT user_id FROM feat_users WHERE height = 0) y
                 ON x.user_id = y.user_id
               """)
    end

    test "constants don't count" do
      assert %{num_db_columns: 2, num_distinct_db_columns: 1} = features_from("SELECT '1' FROM feat_users")
    end

    test "subqueries" do
      assert %{num_db_columns: 8, num_distinct_db_columns: 3} =
               features_from("""
                 SELECT COUNT(*) FROM (
                   SELECT user_id FROM feat_users GROUP BY user_id, name HAVING sum(height) = 0
                 ) foo
               """)
    end
  end

  describe "num_tables" do
    test "simple query" do
      assert %{num_distinct_tables: 1, num_tables: 1} = features_from("SELECT height FROM feat_users")
    end

    test "implicit join" do
      assert %{num_distinct_tables: 2, num_tables: 2} =
               features_from("""
                 SELECT height FROM feat_users, feat_purchases
                 WHERE feat_users.user_id = feat_purchases.user_id
               """)
    end

    test "explicit join" do
      assert %{num_distinct_tables: 2, num_tables: 2} =
               features_from("""
                 SELECT height
                 FROM feat_users INNER JOIN feat_purchases ON feat_users.user_id = feat_purchases.user_id
               """)
    end

    test "subquery" do
      assert %{num_distinct_tables: 2, num_tables: 2} =
               features_from("""
                 SELECT COUNT(*)
                 FROM (
                   SELECT *
                   FROM feat_users INNER JOIN feat_purchases
                     ON feat_users.user_id = feat_purchases.user_id
                 ) foo
               """)
    end

    test "distinct tables" do
      assert %{num_distinct_tables: 2, num_tables: 3} =
               features_from("""
                 SELECT COUNT(*)
                 FROM feat_users AS a, feat_purchases AS b, feat_purchases AS c
                 WHERE a.user_id = b.user_id AND a.user_id = c.user_id
               """)
    end
  end

  describe "functions" do
    test "no function" do
      assert %{functions: [], top_level_functions: [], subquery_functions: []} =
               features_from("SELECT height FROM feat_users")
    end

    test "function used" do
      assert %{functions: ["abs", "cast"], top_level_functions: ["abs", "cast"], subquery_functions: []} =
               features_from("SELECT abs(height), CAST(height AS text) FROM feat_users")
    end

    test "function used in WHERE" do
      assert %{functions: ["sqrt"], top_level_functions: ["sqrt"], subquery_functions: []} =
               features_from("SELECT * FROM feat_users WHERE sqrt(height) = 10")
    end

    test "nested functions used" do
      assert %{functions: ["sqrt", "min"], top_level_functions: ["sqrt", "min"], subquery_functions: []} =
               features_from("SELECT min(sqrt(height)) FROM feat_users")
    end

    test "subqueries" do
      assert %{functions: ["sqrt"], top_level_functions: [], subquery_functions: ["sqrt"]} =
               features_from("SELECT * FROM (SELECT sqrt(height) FROM feat_users) x")
    end

    test "deduplicates functions used" do
      assert %{functions: ["min"], top_level_functions: ["min"], subquery_functions: ["min"]} =
               features_from("SELECT min(height) FROM (SELECT min(height) AS height FROM feat_users) foo")
    end
  end

  describe "select_functions" do
    test "no function" do
      assert %{top_level_select_functions: [], subquery_select_functions: [], select_functions: []} =
               features_from("SELECT height FROM feat_users")
    end

    test "function used" do
      assert %{
               top_level_select_functions: ["abs"],
               subquery_select_functions: ["sqrt"],
               select_functions: ["abs", "sqrt"]
             } = features_from("SELECT abs(foo) FROM (SELECT sqrt(height) AS foo FROM feat_users) x")
    end

    test "deduplicates" do
      assert %{top_level_select_functions: ["sqrt"], subquery_select_functions: ["sqrt"], select_functions: ["sqrt"]} =
               features_from("SELECT sqrt(x) FROM (SELECT sqrt(height) AS x FROM feat_users) foo")
    end

    test "function used in WHERE" do
      assert %{top_level_select_functions: [], subquery_select_functions: [], select_functions: []} =
               features_from("SELECT * FROM feat_users WHERE sqrt(height) = 10")
    end
  end

  describe "filters" do
    test "no filters" do
      assert %{filters: [], top_level_filters: [], subquery_filters: []} =
               features_from("SELECT count(*) FROM feat_users")
    end

    test "filter in HAVING" do
      assert %{filters: ["(= (avg col) const)"], top_level_filters: ["(= (avg col) const)"], subquery_filters: []} =
               features_from("SELECT COUNT(*) FROM feat_users GROUP BY name HAVING AVG(height) = 0")
    end

    test "subqueries" do
      assert %{
               filters: ["(= (avg col) const)", "(<> col const)"],
               top_level_filters: ["(= (avg col) const)"],
               subquery_filters: ["(<> col const)"]
             } =
               features_from("""
                 SELECT COUNT(*) FROM (
                   SELECT user_id, height
                   FROM feat_users
                   WHERE height <> 0
                 ) foo
                 GROUP BY foo.height
                 HAVING AVG(foo.height) = 0
               """)
    end

    test "extracts filter structure" do
      assert MapSet.new(["(< col const)", "(>= col const)"]) ==
               features_from("""
                 SELECT height
                 FROM feat_users
                 WHERE height > 10 and height < 20
               """).filters
               |> Enum.into(MapSet.new())

      assert MapSet.new(["(<> col const)", "(= col const)"]) ==
               features_from("""
                 SELECT height
                 FROM feat_users
                 WHERE height <> 10 and male = true
               """).filters
               |> Enum.into(MapSet.new())

      assert MapSet.new(["(in col set)", "(<> col const)"]) ==
               features_from("""
                 SELECT height
                 FROM feat_users
                 WHERE
                   height IN (10, 11) and height NOT IN (12, 13) and
                   name IN ('bob', 'alice')
               """).filters
               |> Enum.into(MapSet.new())

      assert MapSet.new(["(is-null col)", "(not (is-null col))"]) ==
               features_from("""
                 SELECT height
                 FROM feat_users
                 WHERE height IS NULL and name IS NOT NULL
               """).filters
               |> Enum.into(MapSet.new())

      assert MapSet.new([
               "(like col const)",
               "(ilike col const)",
               "(not (like col const))",
               "(not (ilike col const))"
             ]) ==
               features_from("""
                 SELECT height
                 FROM feat_users
                 WHERE name LIKE '%' and name ILIKE '%foo%' and name NOT LIKE '_' and name NOT ILIKE '%bar%'
               """).filters
               |> Enum.into(MapSet.new())
    end
  end

  describe "number of group by clauses" do
    test "no group by" do
      assert %{num_top_level_group_by: 0, num_subquery_group_by: 0, num_group_by: 0} =
               features_from("SELECT count(*) FROM feat_users")
    end

    test "top-level group by" do
      assert %{num_top_level_group_by: 1, num_subquery_group_by: 0, num_group_by: 1} =
               features_from("SELECT count(*) FROM feat_users GROUP BY height")
    end

    test "subquery group by" do
      assert %{num_top_level_group_by: 0, num_subquery_group_by: 1, num_group_by: 1} =
               features_from("SELECT count(*) FROM (SELECT count(*) FROM feat_users GROUP BY height) foo")
    end
  end

  test "requested column types" do
    assert %{column_types: ["integer", "text"]} = features_from("SELECT height, name FROM feat_users")
  end

  test "requested column types - works across tables" do
    assert %{column_types: ["integer", "datetime"]} =
             features_from("""
               SELECT height, datetime
               FROM feat_users, feat_purchases
               WHERE feat_users.user_id = feat_purchases.user_id
             """)
  end

  test "requested column types - types from constants" do
    assert %{column_types: ["text", "integer"]} = features_from("SELECT 'string', 1 FROM feat_users")
  end

  test "requested column types - types from functions" do
    assert %{column_types: ["integer"]} = features_from("SELECT count(distinct height) FROM feat_users")
  end

  test "returns type of selected columns - when constant" do
    assert %{selected_types: ["integer"]} = features_from("SELECT 1 FROM feat_users")
  end

  test "returns type of selected columns - when column" do
    assert %{selected_types: ["integer"]} = features_from("SELECT height FROM feat_users")
  end

  test "returns type of selected columns - when function" do
    assert %{selected_types: ["integer"]} = features_from("SELECT length(name) FROM feat_users")
  end

  describe "features->expressions" do
    test "includes representations of expressions used" do
      assert ["(min (+ const (sqrt col)))", "(+ col const)", "const"] =
               features_from("SELECT min(1 + sqrt(height)) FROM feat_users WHERE height + 1 = 2").expressions
    end

    test "resolves references into subqueries" do
      assert ["(min (+ col const))"] =
               features_from("SELECT min(x) FROM (SELECT user_id, height + 1 AS x FROM feat_users) foo").expressions
    end

    test "distinct" do
      assert ["(count (distinct col))"] = features_from("SELECT count(distinct height) FROM feat_users").expressions
    end

    test "*" do
      assert ["(count *)"] = features_from("SELECT count(*) FROM feat_users").expressions
    end
  end

  test "successful view validation" do
    assert {:ok, [col1, col2]} = validate_view("v1", "select user_id, name from feat_users")
    assert col1 == %{name: "user_id", type: "text", user_id: true}
    assert col2 == %{name: "name", type: "text", user_id: false}
  end

  test "successful validation of a view which uses another view" do
    assert {:ok, [col1, col2]} =
             validate_view("v1", "select user_id, name from table_view", %{
               "table_view" => "select user_id, name from feat_users"
             })

    assert col1 == %{name: "user_id", type: "text", user_id: true}
    assert col2 == %{name: "name", type: "text", user_id: false}
  end

  test "view can't have the same name as the table",
    do: assert({:error, :name, "has already been taken"} == validate_view("feat_users", ""))

  test "describe query with early binding" do
    assert {:ok, columns, capabilities} = describe_query("select $1 from feat_users", [%{type: :boolean, value: true}])

    assert columns == [""]
    assert capabilities.selected_types == ["boolean"]
    assert capabilities.parameter_types == ["boolean"]
  end

  test "describe query with late binding" do
    assert {:ok, columns, capabilities} = describe_query("select cast($1 as boolean) from feat_users")

    assert columns == [""]
    assert capabilities.selected_types == ["boolean"]
    assert capabilities.parameter_types == ["boolean"]
  end

  test "describe query with late binding in a subquery" do
    assert {:ok, columns, capabilities} =
             describe_query("select x from (select user_id, cast($1 as boolean) as x from feat_users) sq")

    assert columns == ["x"]
    assert capabilities.selected_types == ["boolean"]
    assert capabilities.parameter_types == ["boolean"]
  end

  test "describing select *" do
    assert {:ok, columns, capabilities} = describe_query("select * from feat_describe")
    assert columns == ["id", "t", "f", "b", "d"]
    assert capabilities.selected_types == ["integer", "text", "real", "boolean", "datetime"]
  end

  test "late bound parameters must be casted" do
    assert {:error, error} = describe_query("select $1 from feat_users")
    assert error == "The type for parameter `$1` cannot be determined."
  end

  test "all parameters must be supplied" do
    assert {:error, error} = describe_query("select cast($2 as boolean) from feat_users")
    assert error == "The type for parameter `$1` cannot be determined."
  end

  test "db_columns resolving simple column" do
    uid_column = Table.column("uid", :integer)
    string_column = Table.column("string", :text)
    table = Table.new("table", "uid", columns: [uid_column, string_column])

    query = %Query{
      command: :select,
      columns: [Expression.column(string_column, table)],
      column_titles: ["string"],
      selected_tables: [table],
      from: "table",
      type: :anonymized
    }

    assert [
             %Expression{name: "uid", row_index: 0},
             %Expression{name: "string", row_index: 1}
           ] = Query.resolve_db_columns(query).db_columns
  end

  test "db_columns resolving function call" do
    uid_column = Table.column("uid", :integer)
    numeric_column = Table.column("numeric", :integer)
    table = Table.new("table", "uid", columns: [uid_column, numeric_column])

    query = %Query{
      command: :select,
      columns: [Expression.function("abs", [Expression.column(numeric_column, table)], :integer)],
      column_titles: ["abs"],
      selected_tables: [table],
      from: "table",
      type: :anonymized
    }

    assert [
             %Expression{name: "uid", row_index: 0},
             %Expression{name: "numeric", row_index: 1}
           ] = Query.resolve_db_columns(query).db_columns
  end

  test "offloaded where clauses extraction" do
    uid_column = Table.column("uid", :integer)
    numeric_column = Table.column("numeric", :integer)
    table = Table.new("table", "uid", columns: [uid_column, numeric_column])

    condition = {:comparison, Expression.column(numeric_column, table), :=, Expression.constant(:integer, 3)}

    query = %Query{command: :select, where: condition, selected_tables: [table], from: "table"}

    assert ^condition = Query.offloaded_where(query)
    assert nil == Query.emulated_where(query)
  end

  test "emulated where clauses extraction" do
    uid_column = Table.column("uid", :integer)
    text_column = Table.column("text", :text)
    table = Table.new("table", "uid", columns: [uid_column, text_column])

    decoded_column = Expression.function("dec_b64", [Expression.column(text_column, table)], :text)
    condition = {:comparison, decoded_column, :=, Expression.constant(:text, "a")}

    query = %Query{
      command: :select,
      where: condition,
      selected_tables: [table],
      from: "table",
      data_source: hd(Cloak.DataSource.all())
    }

    assert ^condition = Query.emulated_where(query)
    assert nil == Query.offloaded_where(query)
  end

  test "[Issue #2815] bucket type" do
    assert ["real"] = features_from("SELECT BUCKET(height BY 10) FROM feat_users").selected_types
  end

  defp describe_query(statement, parameters \\ nil),
    do: Query.describe_query(hd(Cloak.DataSource.all()), statement, parameters, %{})

  defp validate_view(name, sql, views \\ %{}) do
    [first_ds | rest_ds] = Cloak.DataSource.all()
    result = Query.validate_view(first_ds, name, sql, views)
    Enum.each(rest_ds, &assert(result == Query.validate_view(&1, name, sql, views)))
    result
  end

  defp features_from(statement) do
    [first_ds | rest_ds] = Cloak.DataSource.all()
    {query, features} = compile_with_features(first_ds, statement)
    query = query |> Map.drop([:features]) |> scrub_data_sources()

    for data_source <- rest_ds do
      {other_query, other_features} = compile_with_features(data_source, statement)
      other_query = other_query |> Map.drop([:features]) |> scrub_data_sources()

      assert query == other_query

      assert Map.drop(features, [:driver, :driver_dialect]) == Map.drop(other_features, [:driver, :driver_dialect])
    end

    features
  end

  defp compile_with_features(data_source, statement) do
    {:ok, parsed_query} = Parser.parse(statement)

    {:ok, query} = Compiler.compile(parsed_query, data_source, _parameters = [], _views = %{})

    features = Query.features(query)

    {scrub_data_sources(query), features}
  end
end
