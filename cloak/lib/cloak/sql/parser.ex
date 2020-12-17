defmodule Cloak.Sql.Parser do
  @moduledoc "Parser for SQL queries."

  alias Cloak.Sql.Parser.ASTNormalization

  @type unqualified_identifier :: {:quoted, String.t()} | {:unquoted, String.t()}

  @type location :: {integer, integer} | nil

  @type qualified_identifier :: {:identifier, :unknown | table, unqualified_identifier, location}

  @type data_type :: Cloak.DataSource.data_type() | :interval

  @type function_name ::
          String.t()
          | {:bucket, atom}
          | {:cast, data_type}
          | %{canonical_name: String.t(), synonym_used: String.t()}

  @type constant :: {:constant, data_type, any, location}

  @type column ::
          qualified_identifier
          | {:distinct, qualified_identifier}
          | function_spec
          | constant
          | {:parameter, pos_integer}

  @type function_spec :: {:function, function_name, [column], location}

  @type filter_clause :: nil | column

  @type from_clause :: table | subquery | join

  @type table :: unqualified_identifier | {unqualified_identifier, :as, String.t()}

  @type join ::
          {:join,
           %{
             type: :cross_join | :inner_join | :full_outer_join | :left_outer_join | :right_outer_join,
             lhs: from_clause,
             rhs: from_clause,
             condition: filter_clause
           }}

  @type subquery :: {:subquery, %{ast: select | union, alias: String.t()}}

  @type select ::
          %{
            command: :select,
            columns: [column | {column, :as, String.t()} | {:*, String.t()} | :*],
            grouping_sets: [[column]],
            from: from_clause,
            where: filter_clause,
            order_by: [{column, :asc | :desc, :nulls_first | :nulls_last | :nulls_natural}],
            having: filter_clause,
            limit: integer,
            offset: integer,
            distinct?: boolean
          }

  @type show ::
          %{
            command: :show,
            from: table,
            show: :tables | :columns
          }

  @type union :: %{
          command: :union,
          distinct?: boolean,
          from: {:union, subquery, subquery}
        }

  @type explain ::
          %{
            command: {:explain, :select | :union},
            from: select
          }

  @type parsed_query :: select | show | explain | union

  @combine_error_regex ~r/(?<error>.*) at line (?<line>\d+), column (?<column>\d+)/

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses a and normalizes an SQL query in text form. Raises on error."
  @spec parse!(String.t()) :: parsed_query
  def parse!(string) do
    case parse(string) do
      {:ok, query} ->
        query

      {:error, error} ->
        case Regex.named_captures(@combine_error_regex, error) do
          %{"error" => simple_error, "line" => line, "column" => column} ->
            raise Cloak.Sql.Parser.ParseError,
              message: "#{simple_error}.",
              source_location: {String.to_integer(line), String.to_integer(column)}

          _ ->
            raise Cloak.Sql.Parser.ParseError, message: error
        end
    end
  end

  @doc "Parses and normalizes an SQL query in text form."
  @spec parse(String.t()) :: {:ok, parsed_query} | {:error, any}
  def parse(statement),
    do: with({:ok, ast} <- Cloak.Sql.Parser.Internal.parse(statement), do: {:ok, ASTNormalization.normalize(ast)})
end
