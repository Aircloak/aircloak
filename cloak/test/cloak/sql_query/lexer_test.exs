defmodule Cloak.SqlQuery.Lexer.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Lexer
  alias Cloak.SqlQuery.Parsers.Token

  test "lexing strings with escaped quotes" do
    assert {:ok, [%Token{category: :constant, value: %{type: :string, value: "a string with a '"}} | _]} =
      Lexer.tokenize("'a string with a \\''")
  end

  test "lexing strings with escaped backslashes" do
    assert {:ok, [%Token{category: :constant, value: %{type: :string, value: "a string with a \\"}} | _]} =
      Lexer.tokenize("'a string with a \\\\'")
  end
end
