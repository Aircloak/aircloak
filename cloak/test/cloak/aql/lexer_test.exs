defmodule Cloak.Aql.Lexer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Lexer
  alias Cloak.Aql.Parsers.Token

  test "lexing strings with escaped quotes" do
    assert {:ok, [%Token{category: :constant, value: %{type: :string, value: "a string with a '"}} | _]} =
      Lexer.tokenize("'a string with a '''")
  end

  test "lexing strings with backslash" do
    assert {:ok, [%Token{category: :constant, value: %{type: :string, value: "a string with a \\"}} | _]} =
      Lexer.tokenize("'a string with a \\'")
  end

  test  "lexing strings with whitespace" do
    assert {:ok, [%Token{category: :from}, %Token{category: :select} | _]} =
      Lexer.tokenize("from\t\n select")
  end

  test "lexing pseudo-identifiers starting with numbers" do
    refute match?({:ok, [%Token{category: :constant} | _]}, Lexer.tokenize("123thing"))
  end
end
