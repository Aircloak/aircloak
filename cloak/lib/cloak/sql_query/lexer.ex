defmodule Cloak.SqlQuery.Lexer do
  @moduledoc "Lexer for SQL queries"
  use Combine
  import Cloak.SqlQuery.Parsers
  alias Cloak.SqlQuery.Parsers.Token

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Tokenizes the given string, and raises on error."
  @spec tokenize!(String.t) :: Token.t
  def tokenize!(query_string) do
    {:ok, tokens} = tokenize(query_string)
    tokens
  end

  @doc "Tokenizes the given string."
  @spec tokenize(String.t) :: {:ok, Token.t} | {:error, any}
  def tokenize(query_string) do
    case Combine.parse(query_string, lexer()) do
      {:error, _} = error -> error
      [tokens] -> {:ok, tokens}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @keywords [
    "SELECT",
    "SHOW",
    "TABLES",
    "COLUMNS",
    "FROM",
    "COUNT",
    "WHERE",
    "AND",
    "LIKE",
    "IN",
    "(", ")",
    ",", ".", ";",
    "<=", ">=", "<>", "=", "<", ">",
    "*",
  ]

  defp lexer() do
    many(choice([
      whitespace(),
      constant(),
      identifier(),
      keyword()
    ]))
    |> eof()
    |> if_then_else(noop(), fail("Invalid character"))
  end

  defp whitespace() do
    ignore(word_of(~r/(\s|\t|\n|\r)+/))
  end

  defp constant() do
    choice([
      numeric_constant(),
      string_constant(),
      boolean_constant()
    ])
  end

  defp numeric_constant() do
    # We use regex to extract any valid numeric form (int, float, float with exp notation)
    # Then we use Integer.parse and Float.parse to convert to a type, favoring int if possible.
    word_of(~r/[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?+/)
    |> map(&{Integer.parse(&1), Float.parse(&1)})
    |> map(fn
          {{int_value, ""}, _} -> {:constant, %{type: :integer, value: int_value}}
          {_, {float_value, ""}} -> {:constant, %{type: :float, value: float_value}}
        end)
    |> output_token()
  end

  defp string_constant() do
    sequence([
      char(?'),
      word_of(~r/[^']*/),
      char(?')
    ])
    |> map(&Enum.join/1)
    |> output_constant(:string)
  end

  defp boolean_constant() do
    boolean_constants = %{"true" => true, "yes" => true, "false" => false, "no" => false}

    choice([
      word_of(~r/true/i),
      word_of(~r/yes/i),
      word_of(~r/false/i),
      word_of(~r/no/i)
    ])
    |> map(&String.downcase/1)
    |> map(&Map.fetch!(boolean_constants, &1))
    |> output_constant(:boolean)
  end

  defp output_constant(parser, type) do
    parser
    |> map(&{:constant, %{type: type, value: &1}})
    |> output_token()
  end

  # precompile regexes so we don't have to do it on every query invocation
  @keyword_regexes Enum.map(@keywords, &Regex.compile!(Regex.escape(&1), "i"))

  defp keyword() do
    choice(Enum.map(@keyword_regexes, &word_of/1))
    |> map(&{:keyword, String.to_atom(String.downcase(&1))})
    |> output_token()
  end

  defp identifier() do
    word_of(~r/[a-zA-Z_][a-zA-Z0-9_]*/)
    |> satisfy(fn(identifier) ->
          not Enum.any?(@keywords, &(&1 == String.upcase(identifier)))
        end)
    |> map(&{:identifier, &1})
    |> output_token()
  end

  defp output_token(token_parser) do
    pair_both(position(), token_parser)
    |> map(fn({{line, column}, {category, value}}) ->
          %Token{line: line, column: column, category: category, value: value}
        end)
  end
end
