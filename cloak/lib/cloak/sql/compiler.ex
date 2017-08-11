defmodule Cloak.Sql.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.Sql.{CompilationError, Expression, Query}
  alias __MODULE__


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(DataSource.t, Parser.parsed_query, [Query.parameter] | nil, Query.view_map) ::
    {:ok, Query.t} | {:error, String.t}
  def compile(data_source, parsed_query, parameters, views) do
    try do
      {:ok,
        parsed_query
        |> Compiler.Specification.compile(data_source, parameters, views)
        |> Compiler.Execution.prepare()
        |> Compiler.Features.compile()
        |> Compiler.Normalization.normalize()
      }
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end

  @doc "Validates a user-defined view."
  @spec validate_view(DataSource.t, Parser.parsed_query, Query.view_map) :: :ok | {:error, String.t}
  def validate_view(data_source, parsed_query, views), do:
    compile(data_source, Map.put(parsed_query, :subquery?, true), [], views)

  @doc "Creates the query which describes a SELECT statement from a single table."
  @spec make_select_query(DataSource.t, DataSource.Table.t, [Expression.t]) :: Query.t
  def make_select_query(data_source, table, select_expressions), do:
    Compiler.Execution.make_select_query(data_source, table, select_expressions)
end
