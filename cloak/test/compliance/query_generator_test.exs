defmodule Cloak.Compliance.QueryGenerator.Test do
  use ExUnit.Case, async: true

  alias Cloak.Compliance.QueryGenerator

  test "smoke test to make sure QueryGenerator works if cloak internals change" do
    tables = [
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

    for complexity <- 0..100 do
      assert QueryGenerator.ast_to_sql(QueryGenerator.generate_ast(tables, complexity))
    end
  end
end
