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

  @type column :: String.t | {:count, :star}

  @type t :: %{
    command: :select | :show,
    columns: [column],
    group_by: [String.t],
    from: [String.t],
    where: [
        {:comparison, String.t, comparator, any}
      | {:like, String.t, String.t}
      | {:not_like, String.t, String.t}
      | {:in, String.t, [any]}
    ],
    order_by: [{String.t, :asc | :desc}],
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
    with {:ok, tokens} <- Cloak.SqlQuery.Lexer.tokenize(query_string) do
      case parse_tokens(tokens, parser()) do
        {:error, _} = error -> error
        [statement] -> {:ok, statement}
      end
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
    switch([
      {keyword(:select), select_statement()},
      {keyword(:show), show_statement()},
      {:else, error_message(fail(""), "Expected `select or show`")}
    ])
    |> map(&create_reportable_map/1)
  end

  defp create_reportable_map({[command], [statement_data]}) do
    statement_data
    |> Enum.reject(fn(value) -> value == nil end)
    |> Map.new
    |> Map.merge(%{command: command})
  end

  defp statement_termination(parser) do
    parser
    |> skip(keyword(:";"))
  end

  defp show_statement() do
    switch([
      {keyword(:tables), noop()},
      {keyword(:columns), from()},
      {:else, fail("Expected `tables or columns`")}
    ])
    |> map(fn({[show], data}) -> [{:show, show} | data] end)
  end

  defp select_statement() do
    sequence([
      select_columns(),
      from(),
      optional_where(),
      optional_group_by(),
      optional_order_by()
    ])
  end

  defp select_columns() do
    map(comma_delimited(column()), &{:columns, &1})
  end

  defp column() do
    either(count_expression(), identifier())
  end

  defp count_expression() do
    keywords([:count, :"(", :"*", :")"])
    |> map(fn(_) -> {:count, :star} end)
  end

  defp from(parser \\ noop()) do
    pair_both(parser,
      keyword(:from),
      from_table_name()
    )
  end

  defp from_table_name() do
    either(table_with_schema(), identifier())
    |> label("table name")
  end

  defp table_with_schema() do
    pipe(
      [identifier(), keyword(:"."), identifier()],
      &Enum.join/1
    )
  end

  defp optional_where() do
    switch([
      {keyword(:where), where_expressions()},
      {:else, noop()}
    ])
    |> map(fn
          {[:where], [where_expressions]} -> {:where, where_expressions}
          other -> other
        end)
  end

  defp where_expressions() do
    and_delimited(where_expression())
  end

  defp where_expression() do
    switch([
      {identifier() |> option(keyword(:not)) |> keyword(:like),
        constant(:string)},
      {identifier() |> keyword(:in),
        in_values()},
      {identifier() |> comparator(),
        allowed_where_values()},
      {:else, fail("Invalid where expression")}
    ])
    |> map(fn
          {[identifier, nil, :like], [string_constant]} -> {:like, identifier, string_constant}
          {[identifier, :not, :like], [string_constant]} -> {:not_like, identifier, string_constant}
          {[identifier, :in], [in_values]} -> {:in, identifier, in_values}
          {[identifier, comparator], [value]} -> {:comparison, identifier, comparator, value}
        end)
  end

  defp in_values() do
    pipe(
      [keyword(:"("), comma_delimited(allowed_where_values()), keyword(:")")],
      fn([_, values, _]) -> values end
    )
  end

  defp allowed_where_values() do
    constant_of([:string, :integer, :float, :boolean])
    |> label("comparison value")
  end

  defp constant_of(expected_types) do
    choice(Enum.map(expected_types, &constant/1))
    |> label(expected_types |> Enum.map(&"#{&1} constant") |> Enum.join(" or "))
  end

  defp constant(expected_type) do
    token(:constant)
    |> satisfy(fn(token) -> token.value.type == expected_type end)
    |> label("#{expected_type} constant")
  end

  defp optional_order_by() do
    switch([
      {keyword(:order), keyword(:by) |> comma_delimited(order_by_field())},
      {:else, noop()}
    ])
    |> map(fn({[:order], [:by, fields]}) -> {:order_by, fields} end)
  end

  defp optional_group_by() do
    switch([
      {keyword(:group), keyword(:by) |> comma_delimited(identifier())},
      {:else, noop()}
    ])
    |> map(fn {_, [:by, columns]} -> {:group_by, columns} end)
  end

  defp order_by_field() do
    pair_both(
      column(),
      order_by_direction()
    )
  end

  defp order_by_direction() do
    option(
      either(
        keyword(:asc),
        keyword(:desc)
      )
    )
  end

  defp identifier() do
    token(:identifier)
    |> map(&(&1.value))
    |> label("identifier")
  end

  defp comparator(parser) do
    parser
    |> keyword_of([:"=", :"<", :"<=", :">=", :">", :"<>"])
    |> label("comparator")
  end

  defp keyword_of(parser, types) do
    parser
    |> choice(Enum.map(types, &keyword(&1)))
    |> label(types |> Enum.join(" or "))
  end

  defp keyword(parser \\ noop(), type) do
    parser
    |> token(type)
    |> map(&(&1.category))
    |> label(to_string(type))
  end

  defp keywords(types) do
    sequence(Enum.map(types, &keyword/1))
  end

  defp comma_delimited(previous \\ noop(), term_parser) do
    previous
    |> sep_by1(term_parser, keyword(:","))
  end

  defp and_delimited(term_parser) do
    sep_by1(term_parser, keyword(:and))
  end

  defp end_of_input(parser), do: parser |> end_of_tokens()


  # -------------------------------------------------------------------
  # Work around invalid combine spec (see below)
  # -------------------------------------------------------------------

  # Temporary hack, since per spec Combine.parse accepts only string, which
  # leads to many dialyzer errors. A couple of functions are copy-pasted here
  # from combine. Once our changes are merged upstream, we should replace this
  # with a regular combine

  defp parse_tokens(tokens, parser) do
    alias Combine.ParserState

    case parser.(%ParserState{input: tokens}) do
      %ParserState{status: :ok, results: res} ->
        res |> Enum.reverse |> Enum.filter_map(&ignore_filter/1, &filter_ignores/1)
      %ParserState{error: res} ->
        {:error, res}
      x ->
        {:error, {:fatal, x}}
    end
  end

  defp ignore_filter(:__ignore), do: false
  defp ignore_filter(_), do: true

  defp filter_ignores(element) when is_list(element) do
    Enum.filter_map(element, &ignore_filter/1, &filter_ignores/1)
  end
  defp filter_ignores(element), do: element
end
