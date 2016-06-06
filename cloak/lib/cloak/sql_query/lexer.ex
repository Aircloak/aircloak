defmodule Cloak.SqlQuery.Lexer do
  @moduledoc "Lexer for SQL queries"
  use Combine
  import Cloak.SqlQuery.Parsers
  alias Cloak.SqlQuery.Parsers.Token

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Tokenizes the given string, and raises on error."
  @spec tokenize!(String.t) :: [Token.t]
  def tokenize!(query_string) do
    {:ok, tokens} = tokenize(query_string)
    tokens
  end

  @doc "Tokenizes the given string."
  @spec tokenize(String.t) :: {:ok, [Token.t]} | {:error, any}
  def tokenize(query_string) do
    case Combine.parse(query_string, lexer()) do
      {:error, _} = error -> error
      [{tokens, eof}] -> {:ok, tokens ++ [eof]}
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
    "NOT",
    "LIKE",
    "ILIKE",
    "IN",
    "ORDER",
    "GROUP",
    "BY",
    "ASC",
    "DESC",
    "(", ")",
    ",", ".", ";",
    "<=", ">=", "<>", "=", "<", ">",
    "*",
  ]

  defp lexer() do
    pair_both(tokens(), eof_token())
  end

  defp tokens() do
    many(choice([
      whitespace(),
      constant(),
      identifier(),
      keyword()
    ]))
  end

  defp eof_token() do
    error_message(eof(), "Invalid character")
    |> position()
    |> map(fn({line, column}) -> %Token{line: line, column: column, category: :eof} end)
  end

  defp whitespace() do
    ignore(
      either(
        spaces(),
        newline() |> increment_line()
      )
    )
  end

  defp constant() do
    choice([
      float_constant(),
      integer_constant(),
      string_constant(),
      boolean_constant()
    ])
  end

  defp float_constant() do
    word_of(~r/[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?+/)
    |> map(&Float.parse/1)
    |> satisfy(&match?({_, ""}, &1))
    |> map(fn({value, _}) -> value end)
    |> output_constant(:float)
  end

  defp integer_constant() do
    word_of(~r/[-+]?[0-9]+/)
    |> map(&Integer.parse/1)
    |> satisfy(&match?({_, ""}, &1))
    |> map(fn({value, _}) -> value end)
    |> output_constant(:integer)
  end

  defp string_constant() do
    sequence([
      ignore(char(?')),
      word_of(~r/[^']*/),
      ignore(char(?'))
    ])
    |> map(&Enum.join/1)
    |> output_constant(:string)
  end

  defp boolean_constant() do
    choice([
      word_of(~r/true/i),
      word_of(~r/false/i)
    ])
    |> map(&String.downcase/1)
    |> map(&String.to_atom/1)
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
    |> map(&{String.to_atom(String.downcase(&1)), nil})
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
