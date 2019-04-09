defmodule Cloak.Sql.Lexer do
  @moduledoc "Lexer for SQL queries"
  import Combine.Parsers.Base
  import Combine.Parsers.Text
  import Cloak.Sql.Parser.Parsers
  alias Cloak.Sql.Parser.Parsers.Token

  @keywords [
              "SELECT",
              "SHOW",
              "TABLES",
              "COLUMNS",
              "FROM",
              "INNER",
              "OUTER",
              "LEFT",
              "RIGHT",
              "FULL",
              "JOIN",
              "ON",
              "CROSS",
              "WHERE",
              "AND",
              "NOT",
              "OR",
              "CAST",
              "BUCKET",
              "ALIGN",
              "LIKE",
              "ILIKE",
              "ESCAPE",
              "IN",
              "IS",
              "BETWEEN",
              "ORDER",
              "GROUP",
              "BY",
              "NULLS",
              "ASC",
              "DESC",
              "AS",
              "NULL",
              "TRUE",
              "FALSE",
              "DISTINCT",
              "ALL",
              "EXTRACT",
              "TRIM",
              "BOTH",
              "LEADING",
              "TRAILING",
              "SUBSTRING",
              "FOR",
              "||",
              "+",
              "-",
              "/",
              "%",
              "^",
              "(",
              ")",
              ",",
              ".",
              ";",
              "::",
              "<=",
              ">=",
              "<>",
              "=",
              "<",
              ">",
              "*",
              "HAVING",
              "LIMIT",
              "OFFSET",
              "SAMPLE_USERS",
              "GROUPING",
              "SETS",
              "CUBE",
              "ROLLUP"
            ]
            # Longer keywords have to be checked first to avoid false matches.
            |> Enum.sort_by(&String.length/1, &>=/2)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Tokenizes the given string."
  @spec tokenize(String.t()) :: {:ok, [Token.t()]} | {:error, any}
  def tokenize(query_string) do
    case Combine.parse(query_string, lexer()) do
      {:error, _} = error -> error
      [{tokens, eof}] -> {:ok, tokens ++ [eof]}
    end
  end

  @doc "Returns a list of SQL keywords accepted by the lexer."
  @spec keywords() :: [String.t()]
  def keywords(), do: @keywords

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp lexer() do
    pair_both(tokens(), eof_token())
  end

  defp tokens() do
    many(
      choice([
        whitespace(),
        multi_line_comment(),
        single_line_comment(),
        constant(),
        quoted_identifier(),
        identifier(),
        keyword(),
        parameter(),
        other()
      ])
    )
  end

  defp eof_token() do
    error_message(eof(), "Invalid character")
    |> pair_both(offset(), position())
    |> map(fn {offset, {line, column}} ->
      %Token{offset: offset, line: line, column: column, category: :eof}
    end)
  end

  defp multi_line_comment(), do: ignore(word_of(~r[/\*.*\*/]Us))

  defp single_line_comment() do
    ignore(
      sequence([
        string("--"),
        word_of(~r/./),
        option(newline())
      ])
    )
  end

  defp parameter(),
    do:
      word_of(~r/\$[1-9][0-9]*/)
      |> map(fn "$" <> index_str -> {:parameter, String.to_integer(index_str)} end)
      |> output_token()

  defp other() do
    char()
    |> map(&{:other, &1})
    |> output_token()
  end

  defp whitespace() do
    ignore(
      either(
        horizontal_whitespace(),
        newline()
      )
    )
  end

  defp horizontal_whitespace() do
    word_of(~r/[\h]/u)
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
    word_of(~r/[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?+/)
    |> map(&Float.parse/1)
    |> satisfy(&match?({_, ""}, &1))
    |> map(fn {value, _} -> value end)
    |> output_constant(:float)
  end

  defp integer_constant() do
    word_of(~r/[0-9]+(?!\w)/)
    |> map(&Integer.parse/1)
    |> satisfy(&match?({_, ""}, &1))
    |> map(fn {value, _} -> value end)
    |> output_constant(:integer)
  end

  defp string_constant() do
    sep_by1(string_part(), many1(whitespace_surrounded_linebreak()))
    |> map(&Enum.join/1)
    |> output_constant(:string)
  end

  defp whitespace_surrounded_linebreak() do
    sequence([
      option(horizontal_whitespace()),
      newline(),
      option(horizontal_whitespace())
    ])
  end

  defp string_part() do
    sequence([
      ignore(char(?')),
      many(string_content()),
      ignore(char(?'))
    ])
    |> map(&Enum.join/1)
  end

  defp string_content() do
    choice([
      pair_right(string("''"), return("'")),
      word_of(~r/[^']+/)
    ])
  end

  defp boolean_constant() do
    choice([
      pair_right(word_of(~r/true/i), return(true)),
      pair_right(word_of(~r/false/i), return(false))
    ])
    |> output_constant(:boolean)
  end

  defp output_constant(parser, type) do
    parser
    |> map(&{:constant, %{type: type, value: &1}})
    |> output_token()
  end

  # precompile regexes so we don't have to do it on every query invocation
  @keyword_regexes Enum.map(@keywords, &Regex.compile!("(#{Regex.escape(&1)}){1}", "i"))

  defp keyword() do
    choice(Enum.map(@keyword_regexes, &word_of/1))
    |> map(&{String.to_atom(String.downcase(&1)), nil})
    |> output_token()
  end

  defp identifier() do
    word_of(~r/[a-zA-Z_#][a-zA-Z0-9_#]*/)
    |> satisfy(fn identifier ->
      not Enum.any?(@keywords, &(&1 == String.upcase(identifier)))
    end)
    |> map(&{:unquoted, &1})
    |> output_token()
  end

  defp quoted_identifier() do
    sequence([
      ignore(char(?")),
      many1(either(escaped_quote(), satisfy(char(), &(&1 != ~s/"/)))),
      ignore(char(?"))
    ])
    |> map(fn [identifier] -> {:quoted, to_string(identifier)} end)
    |> output_token()
  end

  defp escaped_quote(), do: map(string(~s/""/), fn _ -> ~s/"/ end)

  defp output_token(token_parser) do
    sequence([offset(), position(), token_parser])
    |> map(fn [offset, {line, column}, {category, value}] ->
      %Token{offset: offset, line: line, column: column, category: category, value: value}
    end)
  end
end
