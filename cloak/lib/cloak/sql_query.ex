defmodule Cloak.SqlQuery do
  @moduledoc """
  Represents an SQL query.

  This module defines a struct which describes an SQL query. You can use the
  `parse/1` function to convert an SQL query string into a struct.
  """
  use Combine

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
    |> next_token()
    |> eof()
  end

  defp select_statement(parser) do
    parser
    |> select_columns()
    |> from()
  end

  defp select_columns(parser) do
    pair_both(parser,
      keyword(~r/SELECT/i),
      comma_delimited(identifier())
    )
  end

  defp from(parser) do
    pair_both(parser,
      keyword(~r/FROM/i),
      identifier()
    )
  end

  defp identifier() do
    next_token()
    |> word_of(~r/[a-zA-Z_][a-zA-Z0-9_]*/)
  end

  defp keyword(regex) do
    next_token()
    |> word_of(regex)
    |> map(&(&1 |> String.downcase() |> String.to_atom()))
  end

  defp comma_delimited(term_parser) do
    next_token()
    |> sep_by1(next_token(term_parser), char(","))
  end

  defp next_token(), do: skip(whitespaces())

  defp next_token(parser), do: skip(parser, whitespaces())

  defp whitespaces(), do: word_of(~r/(\s|\t|\n|\r)*/)
end
