defmodule Cloak.Aql.Expression.Test do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Expression

  describe "first_column" do
    test "nil if given constant column", do: assert nil == Expression.first_column(%Expression{constant?: true})

    test "first db column if one present" do
      return_column = %Expression{row_index: 1}
      assert return_column == Expression.first_column(Expression.function("f", [
        Expression.function("f", [%Expression{constant?: true}]),
        Expression.function("f", [%Expression{constant?: true}, return_column]),
      ]))
    end
  end
end
