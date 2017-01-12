defmodule Cloak.Aql.QueryTest do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("feat_users", "height INTEGER, name TEXT, male BOOLEAN")
    :ok = Cloak.Test.DB.create_table("feat_purchases", "price INTEGER, name TEXT, datetime TIMESTAMP")
    :ok
  end

  test "extracts number of selected columns" do
    assert %{num_selected_columns: 1} = features_from("SELECT height FROM feat_users")
    assert %{num_selected_columns: 3} = features_from("SELECT height, name, male FROM feat_users")
  end

  test "extracts number of selected columns - duplicates count too" do
    assert %{num_selected_columns: 3} = features_from("SELECT name, name, name FROM feat_users")
  end

  test "extracts number of selected columns - constants count too" do
    assert %{num_selected_columns: 1} = features_from("SELECT '1' FROM feat_users")
  end

  test "extracts number of columns loaded from the database" do
    assert %{num_db_columns: 1} = features_from("SELECT height FROM feat_users")
  end

  test "extracts number of columns loaded from the database - deduplicated" do
    assert %{num_db_columns: 1} = features_from("SELECT height, height FROM feat_users")
  end

  test "extracts number of columns loaded from the database - constants don't count" do
    assert %{num_db_columns: 0} = features_from("SELECT '1' FROM feat_users")
  end

  test "extracts number of selected tables" do
    assert %{num_tables: 1} = features_from("SELECT height FROM feat_users")
    assert %{num_tables: 2} = features_from("""
      SELECT height FROM feat_users, feat_purchases
      WHERE feat_users.user_id = feat_purchases.user_id
    """)
    assert %{num_tables: 2} = features_from("""
      SELECT height
      FROM feat_users INNER JOIN feat_purchases ON feat_users.user_id = feat_purchases.user_id
    """)
  end

  test "extracts types of functions used - no function" do
    assert %{functions: []} = features_from("SELECT height FROM feat_users")
    assert %{functions: []} = features_from("SELECT * FROM feat_users")
  end

  test "extracts types of functions used - function used" do
    assert %{functions: ["abs", "cast"]} = features_from("SELECT abs(height), CAST(height AS text) FROM feat_users")
  end

  test "extracts types of functions used - multiple functions used" do
    assert %{functions: ["min", "trunc"]} = features_from("SELECT min(trunc(height)) FROM feat_users")
  end

  test "extracts types of functions used - deduplicates functions used" do
    assert %{functions: ["min"]} = features_from("SELECT min(height), min(height) FROM feat_users")
  end

  test "extracts types of where conditions used - no where conditions" do
    assert %{where_conditions: ["not null"]} = features_from("SELECT count(*) FROM feat_users")
  end

  test "extracts types of where conditions used" do
    assert MapSet.new(["<", ">=", "not null"]) == features_from("""
      SELECT height
      FROM feat_users
      WHERE height > 10 and height < 20
    """).where_conditions |> Enum.into(MapSet.new())
    assert MapSet.new(["=", "<>", "not null"]) == features_from("""
      SELECT height
      FROM feat_users
      WHERE height <> 10 and male = true
    """).where_conditions |> Enum.into(MapSet.new())
    assert MapSet.new(["in", "not null", "not in"]) == features_from("""
      SELECT height
      FROM feat_users
      WHERE
        height IN (10, 11) and height NOT IN (12, 13) and
        name IN ('bob', 'alice')
    """).where_conditions |> Enum.into(MapSet.new())
    assert MapSet.new(["null", "not null"]) == features_from("""
      SELECT height
      FROM feat_users
      WHERE height IS NULL and name IS NOT NULL
    """).where_conditions |> Enum.into(MapSet.new())
    assert MapSet.new(["like", "ilike", "not null", "not like", "not ilike"]) == features_from("""
      SELECT height
      FROM feat_users
      WHERE name LIKE '%' and name ILIKE '%foo%' and name NOT LIKE '_' and name NOT ILIKE '%bar%'
    """).where_conditions |> Enum.into(MapSet.new())
  end

  test "num of group by clauses - no group by" do
    assert %{num_group_by: 0} = features_from("SELECT count(*) FROM feat_users")
  end

  test "num of group by clauses - has group by" do
    assert %{num_group_by: 1} = features_from("SELECT count(*) FROM feat_users GROUP BY height")
  end

  test "requested column types" do
    assert %{column_types: ["integer", "text"]} = features_from("SELECT height, name FROM feat_users")
  end

  test "requested column types - works across tables" do
    assert %{column_types: ["integer", "datetime"]} = features_from("""
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

  test "successful view validation" do
    assert {:ok, [col1, col2]} = validate_view("v1", "select user_id, name from feat_users")
    assert col1 == %{name: "user_id", type: "text", user_id: true}
    assert col2 == %{name: "name", type: "text", user_id: false}
  end

  test "successful validation of a view which uses another view" do
    assert {:ok, [col1, col2]} = validate_view("v1", "select user_id, name from table_view",
      %{"table_view" => "select user_id, name from feat_users"})
    assert col1 == %{name: "user_id", type: "text", user_id: true}
    assert col2 == %{name: "name", type: "text", user_id: false}
  end

  test "view can't have the same name as the table", do:
    assert {:error, :name, "has already been taken"} == validate_view("feat_users", "")

  test "describe query with early binding" do
    assert {:ok, columns, capabilities} = describe_query("select $1 from feat_users", [true])
    assert columns == [""]
    assert capabilities.selected_types == ["boolean"]
    assert capabilities.parameter_types == ["boolean"]
  end

  test "describe query with late binding" do
    assert {:ok, columns, capabilities} = describe_query("select cast($1 as boolean) from feat_users")
    assert columns == ["cast"]
    assert capabilities.selected_types == ["boolean"]
    assert capabilities.parameter_types == ["boolean"]
  end

  test "describe query with late binding in a subquery" do
    assert {:ok, columns, capabilities} = describe_query(
      "select x from (select user_id, cast($1 as boolean) as x from feat_users) sq"
    )
    assert columns == ["x"]
    assert capabilities.selected_types == ["boolean"]
    assert capabilities.parameter_types == ["boolean"]
  end

  test "late bound parameters must be casted" do
    assert {:error, error} = describe_query("select $1 from feat_users")
    assert error == "The type for parameter `$1` cannot be determined."
  end

  test "all parameters must be supplied" do
    assert {:error, error} = describe_query("select cast($2 as boolean) from feat_users")
    assert error == "The type for parameter `$1` cannot be determined."
  end

  defp describe_query(statement, parameters \\ nil), do:
    Query.describe_query(hd(Cloak.DataSource.all()), statement, parameters, %{})

  defp validate_view(name, sql, views \\ %{}) do
    [first_ds | rest_ds] = Cloak.DataSource.all()
    result = Query.validate_view(first_ds, name, sql, views)
    Enum.each(rest_ds, &assert(result == Query.validate_view(&1, name, sql, views)))
    result
  end

  defp features_from(statement) do
    [first_ds | rest_ds] = Cloak.DataSource.all()
    query = Query.make!(first_ds, statement, [], %{}) |> Map.delete(:data_source)
    for data_source <- rest_ds, do:
      assert(query == Query.make!(data_source, statement, [], %{}) |> Map.delete(:data_source))
    Query.extract_features(query)
  end
end
