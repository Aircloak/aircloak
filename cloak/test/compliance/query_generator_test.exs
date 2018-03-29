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

    for _ <- 1..100 do
      assert tables |> QueryGenerator.generate_ast() |> QueryGenerator.ast_to_sql()
    end
  end
end
