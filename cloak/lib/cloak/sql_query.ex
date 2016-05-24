defmodule Cloak.SqlQuery do
  @moduledoc """
  Represents an SQL query.

  This module defines a struct which describes an SQL query. You can use the
  `parse/1` function to convert an SQL query string into a struct.
  """
  use Combine
  import Cloak.Combinators

  @type t :: %__MODULE__{
    from: String.t,
    select: [String.t]
  }

  defstruct [:from, :select]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses and returns the query string. Raises on parse error."
  @spec parse!(String.t) :: t
  def parse!(query_string) do
    {:ok, parsed_query} = parse(query_string)
    parsed_query
  end

  @doc "Parses the query string."
  @spec parse(String.t) :: {:ok, t} | {:error, any}
  def parse(query_string) do
    case Combine.parse(query_string, parser()) do
      {:error, _} = error -> error
      query_spec -> {:ok, struct!(__MODULE__, query_spec)}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parser do
    next_token()
    |> select_statement()
    |> statement_termination()
    |> end_of_input()
  end

  defp statement_termination(parser) do
    parser
    |> next_token()
    |> skip(char(?;))
  end

  defp select_statement(parser) do
    parser
    |> select_columns()
    |> from()
  end

  defp select_columns(parser) do
    pair_both(parser,
      expected_keyword(:select),
      comma_delimited(identifier())
    )
  end

  defp from(parser) do
    pair_both(parser,
      expected_keyword(:from),
      from_table_name()
    )
  end

  defp from_table_name() do
    next_token()
    |> either(table_with_schema(), identifier())
  end

  defp table_with_schema() do
    pipe(
      [identifier(), char(?.), identifier()],
      &Enum.join/1
    )
  end

  defp identifier() do
    next_token()
    |> error_on(keyword(), fn([keyword]) -> "Unexpected `#{keyword}`" end)
    |> pair_right(word_of(~r/[a-zA-Z_][a-zA-Z0-9_]*/) |> label("identifier"))
  end

  defp keyword(), do: next_token() |> keyword()

  defp keyword(parser) do
    parser
    |> next_token()
    |> choice([
          word_of(~r/SELECT/i),
          word_of(~r/FROM/i)
        ])
    |> map(&String.downcase/1)
    |> map(&String.to_atom/1)
  end

  defp expected_keyword(type) do
    satisfy(keyword(), &(&1 == type))
    |> label(to_string(type))
  end

  defp comma_delimited(term_parser) do
    next_token()
    |> sep_by1(next_token(term_parser), char(","))
  end

  defp end_of_input(parser), do: parser |> next_token() |> eof()

  defp next_token(), do: skip(whitespaces())

  defp next_token(parser), do: skip(parser, whitespaces())

  defp whitespaces(), do: word_of(~r/(\s|\t|\n|\r)*/)
end
