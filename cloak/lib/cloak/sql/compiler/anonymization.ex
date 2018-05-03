defmodule Cloak.Sql.Compiler.Anonymization do
  @moduledoc """
    This module inspects the AST and sets the validation type for each query (top-level and subqueries),
    in order to ensure that user data is processed correctly.

    A subquery can have the following types:

    - `standard`: an arbitrary SQL query that processes data un-restricted.
      Either the data is not privacy sensitive or the query is the source of the data (virtual tables).

    - `restricted`: an SQL query that selects or aggregates data per-user, as input for an anonymized query, and
      which is subject to Aircloak specific restrictions (aligned ranges, restricted math, etc.).

    - `anonymized`: an SQL query that aggregates per-user, privacy sensitive data into anonymized data. The input
      expressions for the anonymized aggregators are subject to the same Aircloak specific restrictions as `restricted`
      queries are, meaning that, for example, where filters have aligned ranges and restricted math, while having
      filters are un-restricted.
  """

  alias Cloak.Sql.{Compiler.Helpers, Query}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Sets the correct type for each subquery in the AST, depending on how it processes user data."
  @spec compile(Query.t()) :: Query.t()
  def compile(query), do: Helpers.apply_bottom_up(query, &%{&1 | type: get_query_type(&1)})

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp get_query_type(query) do
    if Enum.all?(query.selected_tables, &(&1.user_id == nil)) do
      :standard
    else
      if query.subquery? and Helpers.uid_column_selected?(query) do
        :restricted
      else
        :anonymized
      end
    end
  end
end
