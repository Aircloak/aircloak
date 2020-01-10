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

  describe "selected_types" do
    test "when constant" do
      assert %{selected_types: ["integer"]} = metadata_from("SELECT 1 FROM feat_users")
    end

    test "when column" do
      assert %{selected_types: ["integer"]} = metadata_from("SELECT height FROM feat_users")
    end

    test "when function" do
      assert %{selected_types: ["integer"]} = metadata_from("SELECT length(name) FROM feat_users")
    end
  end

  describe ".max_rare_negative_conditions" do
    test "defaults to the globally configured maximum (2 for tests)" do
      assert 2 == Query.max_rare_negative_conditions(%Query{data_source: %{}})
    end

    test "accepts max rare conditions number configured for the data source" do
      assert 0 == Query.max_rare_negative_conditions(%Query{data_source: %{max_rare_negative_conditions: 0}})
    end
  end

  test "successful view validation" do
    assert {:ok, [col1, col2]} = validate_view("v1", "select user_id, name from feat_users")
    assert col1 == %{name: "user_id", type: "text", key_type: "user_id"}
    assert col2 == %{name: "name", type: "text", key_type: nil}
  end

  test "successful validation of a view which uses another view" do
    assert {:ok, [col1, col2]} =
             validate_view("v1", "select user_id, name from table_view", %{
               "table_view" => "select user_id, name from feat_users"
             })

    assert col1 == %{name: "user_id", type: "text", key_type: "user_id"}
    assert col2 == %{name: "name", type: "text", key_type: nil}
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
    assert ["real"] = metadata_from("SELECT BUCKET(height BY 10) FROM feat_users").selected_types
  end

  defp describe_query(statement, parameters \\ nil),
    do: Query.describe_query(nil, hd(Cloak.DataSource.all()), statement, parameters, %{})

  defp validate_view(name, sql, views \\ %{}) do
    [first_ds | rest_ds] = Cloak.DataSource.all()
    result = Query.validate_view(nil, first_ds, name, sql, views)
    Enum.each(rest_ds, &assert(result == Query.validate_view(nil, &1, name, sql, views)))
    result
  end

  defp metadata_from(statement) do
    [first_ds | rest_ds] = Cloak.DataSource.all()
    {query, metadata} = compile_with_metadata(first_ds, statement)
    query = query |> scrub_data_sources()

    for data_source <- rest_ds do
      {other_query, other_metadata} = compile_with_metadata(data_source, statement)
      other_query = other_query |> scrub_data_sources()

      assert query == other_query

      assert metadata == other_metadata
    end

    metadata
  end

  defp compile_with_metadata(data_source, statement) do
    {:ok, parsed_query} = Parser.parse(statement)

    {:ok, query} = Compiler.compile(parsed_query, nil, data_source, _parameters = [], _views = %{})

    metadata = Query.metadata(query)

    {scrub_data_sources(query), metadata}
  end
end
