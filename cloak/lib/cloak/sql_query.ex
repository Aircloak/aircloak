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
    |> map(&create_reportable_map/1)
  end

  defp create_reportable_map({command, [statement_data]}) do
    statement_data = statement_data
    |> Enum.reject(fn(value) -> value == nil end)
    |> Map.new
    Map.merge(%{command: command}, statement_data)
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
      option(where())
    ])
  end

  defp select_columns() do
    map(comma_delimited(column()), &{:columns, &1})
  end

  defp column() do
    either(count_expression(), identifier())
  end

  defp count_expression() do
    pipe(
      [
        keyword(:count),
        char("("),
        next_token(),
        char("*"),
        next_token(),
        char(")"),
      ],
      fn _ -> {:count, :star} end
    )
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
    pair_both(
      keyword(:where),
      and_delimited(where_expression())
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
    quoted_value(~r/[%\w\s]/)
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
    |> map(&convert_to_boolean/1)
    |> one_of([true, false])
  end

  defp convert_to_boolean(string) when is_bitstring(string) do
    string
    |> String.downcase
    |> map_to_boolean
  end
  defp convert_to_boolean(_non_string), do: :unknown

  defp map_to_boolean("yes"), do: true
  defp map_to_boolean("true"), do: true
  defp map_to_boolean("no"), do: false
  defp map_to_boolean("false"), do: false
  defp map_to_boolean(_), do: :unknown

  defp identifier() do
    next_token()
    |> word_of(~r/[a-zA-Z_][a-zA-Z0-9_]*/)
    |> satisfy(fn(identifier) ->
          not Enum.any?(keyword_matchers(), &(Regex.replace(&1, identifier, "") == ""))
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
    quoted_value(~r/[\w\s]/)
  end

  defp quoted_value(regex) do
    next_token()
    |> pipe([char("'"), word_of(regex), char("'")], &Enum.join/1)
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
      ~r/IN/i,
      ~r/COUNT/i
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
