defmodule Cloak.SqlQuery do
  @moduledoc "Implements a parser of the sql query."
  use Combine
  import Cloak.SqlQuery.Parsers

  @type t :: %{
    statement: :select | :show,
    from: [String.t],
    columns: [String.t],
    show: :tables | :columns
  }


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
      [statement] -> {:ok, statement}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parser do
    statement()
    |> statement_termination()
    |> end_of_input()
  end

  defp statement() do
    switch(
      keyword_of([:select, :show]),
      %{
        select: select_statement(),
        show: show_statement()
      }
    )
    |> map(fn({statement, [statement_data]}) ->
      Map.merge(%{statement: statement}, Map.new(statement_data))
    end)
  end

  defp statement_termination(parser) do
    parser
    |> next_token()
    |> skip(char(?;))
  end

  defp show_statement() do
    switch(
      keyword_of([:tables, :columns]),
      %{
        tables: noop(),
        columns: from()
      }
    )
    |> map(fn({show, data}) -> [{:show, show} | data] end)
  end

  defp select_statement() do
    sequence([
      select_columns(),
      from()
    ])
  end

  defp select_columns() do
    map(comma_delimited(identifier()), &{:columns, &1})
  end

  defp from() do
    next_token()
    |> from()
  end

  defp from(parser) do
    pair_both(parser,
      keyword(:from),
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
    |> word_of(~r/[a-zA-Z_][a-zA-Z0-9_]*/)
    |> satisfy(fn(identifier) ->
          not Enum.any?(keyword_matchers(), &Regex.match?(&1, identifier))
        end)
    |> label("identifier")
  end

  defp keyword_of(types) do
    choice(Enum.map(types, &keyword(&1)))
    |> label(types |> Enum.join(" or "))
  end

  defp keyword(type) do
    next_token()
    |> choice(Enum.map(keyword_matchers(), &word_of/1))
    |> map(&String.downcase/1)
    |> map(&String.to_atom/1)
    |> satisfy(&(&1 == type))
    |> label(to_string(type))
  end

  defp keyword_matchers() do
    [
      ~r/SELECT/i,
      ~r/SHOW/i,
      ~r/TABLES/i,
      ~r/COLUMNS/i,
      ~r/FROM/i
    ]
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
