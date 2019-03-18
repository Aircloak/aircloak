defmodule Cloak.Compliance.QueryGenerator.Test do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Cloak.Compliance.QueryGenerator

  property "converts to SQL without crashing" do
    check all complexity <- integer(0..100) do
      assert QueryGenerator.ast_to_sql(QueryGenerator.generate_ast(tables(), complexity))
    end
  end

  property "converts to SQL + analyst table list without crashing" do
    check all complexity <- integer(0..100) do
      {query, analyst_tables} = QueryGenerator.extract_analyst_tables(QueryGenerator.generate_ast(tables(), complexity))

      assert QueryGenerator.ast_to_sql(query)

      for {_name, definition} <- analyst_tables do
        assert QueryGenerator.ast_to_sql(definition)
      end
    end
  end

  property "remove_analyst_tables creates valid queries with no analyst_tables" do
    check all complexity <- integer(0..100) do
      assert {query, []} =
               tables()
               |> QueryGenerator.generate_ast(complexity)
               |> QueryGenerator.remove_analyst_tables()
               |> QueryGenerator.extract_analyst_tables()

      assert QueryGenerator.ast_to_sql(query)
    end
  end

  defp tables() do
    [
      %{
        name: "table1",
        user_id: "uid",
        columns: [
          %{name: "uid", type: :integer},
          %{name: "column1", type: :integer},
          %{name: "column2", type: :real}
        ]
      },
      %{
        name: "table2",
        user_id: "uid",
        columns: [
          %{name: "uid", type: :integer},
          %{name: "column1", type: :datetime},
          %{name: "column2", type: :text}
        ]
      }
    ]
  end
end
