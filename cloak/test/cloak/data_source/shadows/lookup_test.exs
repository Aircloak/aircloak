defmodule Cloak.DataSource.Shadows.Lookup.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Expression
  alias Cloak.DataSource.Shadows.Lookup

  describe ".any?" do
    test "true if expression matches any value" do
      assert Lookup.any?(column(), 3, [1, 2, 3, 4, 5])
    end

    test "false if expression matches no rows" do
      refute Lookup.any?(column(), 30, [1, 2, 3, 4, 5])
    end

    test "complex expression" do
      expression = Expression.function("sqrt", [column()], :real)
      assert Lookup.any?(expression, 2, [1, 2, 3, 4, 5])
      refute Lookup.any?(expression, 5, [1, 2, 3, 4, 5])
    end

    test "expression with constants" do
      expression = Expression.function("+", [column(), Expression.constant(:integer, 10)], :integer)
      assert Lookup.any?(expression, 13, [1, 2, 3, 4, 5])
      refute Lookup.any?(expression, 2, [1, 2, 3, 4, 5])
    end
  end

  defp column() do
    Expression.column(%{name: "column", type: :integer}, %{name: "table", user_id: "uid"})
  end
end
