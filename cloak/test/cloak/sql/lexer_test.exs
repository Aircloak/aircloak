defmodule Cloak.Sql.Lexer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Lexer
  alias Cloak.Sql.Parsers.Token

  test "lexing strings with escaped quotes" do
    assert {:ok, [%Token{category: :constant, value: %{type: :string, value: "a string with a '"}} | _]} =
             Lexer.tokenize("'a string with a '''")
  end

  test "lexing strings with backslash" do
    assert {:ok,
            [
              %Token{category: :constant, value: %{type: :string, value: "a string with a \\"}}
              | _
            ]} = Lexer.tokenize("'a string with a \\'")
  end

  test "lexing strings with whitespace" do
    assert {:ok, [%Token{category: :from}, %Token{category: :select} | _]} = Lexer.tokenize("from\t\n select")
  end

  test "lexing split strings" do
    assert {:ok,
            [
              %Token{
                category: :constant,
                value: %{type: :string, value: "a string that is split"}
              }
              | _
            ]} = Lexer.tokenize("'a string'\n ' that is' \r\n' split'")
  end

  test "lexing pseudo-identifiers starting with numbers" do
    refute match?({:ok, [%Token{category: :constant} | _]}, Lexer.tokenize("123thing"))
  end
end
