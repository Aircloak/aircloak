defmodule Cloak.Compliance.QueryGenerator.Test do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Cloak.Compliance.QueryGenerator

  property "converts to SQL without crashing" do
    check all complexity <- integer(0..100) do
      assert QueryGenerator.ast_to_sql(QueryGenerator.generate_ast(tables(), complexity))
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
