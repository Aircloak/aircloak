defmodule Cloak.Compliance.QueryGenerator.Test do
  use ExUnit.Case, async: true

  alias Cloak.Compliance.QueryGenerator

  test "smoke test to make sure QueryGenerator works if cloak internals change" do
    tables = [
      %{
        name: "table1",
        columns: [
          %{name: "column1", type: :integer},
          %{name: "column2", type: :real}
        ]
      },
      %{
        name: "table2",
        columns: [
          %{name: "column1", type: :datetime},
          %{name: "column2", type: :text}
        ]
      }
    ]

    for ast <- tables |> QueryGenerator.ast_generator() |> Enum.take(100) do
      assert QueryGenerator.ast_to_sql(ast)
    end
  end
end
