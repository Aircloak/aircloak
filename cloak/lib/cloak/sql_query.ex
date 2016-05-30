defmodule Cloak.SqlQuery do
  @moduledoc "Implements a parser of the sql query."
  use Combine
  import Cloak.SqlQuery.Parsers

  @type comparator ::
      :=
    | :<
    | :<=
    | :>=
    | :>
    | :<>

  @type t :: %{
    command: :select | :show,
    columns: [String.t],
    from: [String.t],
    where: [
        {:comparison, String.t, comparator, any}
      | {:like, String.t, String.t}
      | {:in, String.t, [any]}
    ],
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
    |> map(fn({command, [statement_data]}) ->
      statement_data = statement_data
      |> Enum.reject(fn(value) -> value == nil end)
      |> Map.new
      Map.merge(%{command: command}, statement_data)
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
      from(),
      where()
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

  defp where() do
    option(
      pair_both(
        keyword(:where),
        and_delimited(where_expression())
      )
    )
  end

  defp where_expression() do
    choice([like(), where_in(), comparison()])
  end

  defp like() do
    pipe(
      [identifier(), keyword(:like), wildcard_comparison_value()],
      fn([identifier, _, wildcard_value]) ->
        {:like, identifier, wildcard_value}
      end
    )
  end

  defp wildcard_comparison_value() do
    next_token()
    |> sequence([char("'"), word_of(~r/[%\w\s]/), char("'")])
    |> map(&Enum.join/1)
    |> label("like comparison value")
  end

  defp where_in() do
    pipe(
      [identifier(), keyword(:in), in_values()],
      fn([identifier, _, in_values]) ->
        {:in, identifier, in_values}
      end
    )
  end

  defp in_values() do
    next_token()
    |> pipe(
        [char("("), comma_delimited(allowed_where_values()), char(")")],
        fn([_, values, _]) -> values end
      )
  end

  defp comparison() do
    pipe(
      [identifier(), comparator(), allowed_where_values()],
      fn([identifier, comparator, value]) ->
        {:comparison, identifier, String.to_atom(comparator), value}
      end
    )
  end

  defp allowed_where_values() do
    next_token()
    |> choice([raw_string(), integer(), float(), boolean()])
  end

  defp boolean() do
    word()
    |> map(fn(word) when is_bitstring(word) -> String.downcase(word); (word) -> word end)
    |> map(fn(word) -> Map.get(boolean_map(), word) end)
    |> one_of([true, false])
  end

  defp boolean_map() do
    %{
      "yes" => true,
      "true" => true,
      "no" => false,
      "false" => false
    }
  end

  defp identifier() do
    next_token()
    |> word_of(~r/[a-zA-Z_][a-zA-Z0-9_]*/)
    |> satisfy(fn(identifier) ->
          not Enum.any?(keyword_matchers(), &Regex.match?(&1, identifier))
        end)
    |> label("identifier")
  end

  defp comparator() do
    next_token()
    |> word_of(~r/[<=>]*/)
    |> one_of(["=", "<", "<=", ">=", ">", "<>"])
    |> label("comparator")
  end

  defp raw_string() do
    sequence([char("'"), word_of(~r/[\w\s]/), char("'")])
    |> map(&Enum.join/1)
    |> label("string value")
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
      ~r/FROM/i,
      ~r/WHERE/i,
      ~r/AND/i,
      ~r/LIKE/i,
      ~r/IN/i
    ]
  end

  defp comma_delimited(term_parser) do
    next_token()
    |> sep_by1(next_token(term_parser), char(","))
  end

  defp and_delimited(term_parser) do
    next_token()
    |> sep_by1(next_token(term_parser), keyword(:and))
  end

  defp end_of_input(parser), do: parser |> next_token() |> eof()

  defp next_token(), do: skip(whitespaces())

  defp next_token(parser), do: skip(parser, whitespaces())

  defp whitespaces(), do: word_of(~r/(\s|\t|\n|\r)*/)
end
