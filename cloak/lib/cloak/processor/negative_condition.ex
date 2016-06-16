defmodule Cloak.Processor.NegativeCondition do
  @moduledoc """
  Implements handling of negated LIKE, ILIKE and equality WHERE clauses on the side of the application.
  These need special handling, because a malicious analyst would be able to find out information about
  individuals by adding a condition that would exclude an individual from a result set. Then by comparing
  the result of a query with and without that condition the analyst can find out if that user was in fact
  included in the result set. To avoid this we ignore the condition if it would remove too few users.
  """

  alias Cloak.Processor.Noise
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token

  import Cloak.Type


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Applies or ignore negative conditions in the query to the given rows."
  @spec apply([Property.t], [String.t], Parser.compiled_query) :: [Property.t]
  def apply(rows, columns, %{where_not: clauses}) do
    clauses
    |> Enum.filter(&sufficient_matches?(&1, rows, columns))
    |> Enum.reduce(rows, fn(clause, rows) -> Enum.reject(rows, filter(clause, columns)) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sufficient_matches?(clause, rows, columns) do
    seed = rows
    |> user_ids()
    |> Enum.uniq()
    |> Noise.random_seed()
    rows
    |> Enum.filter(filter(clause, columns))
    |> Enum.uniq_by(&user_id(&1))
    |> Enum.count()
    |> Noise.passes_filter?(seed)
  end

  defp filter({:comparison, column, :=, %Token{value: %{value: value}}}, columns) do
    index = Enum.find_index(columns, &(&1 === column))
    fn(row) -> Enum.at(row, index + 1) == value end
  end
  defp filter({:like, column, %Token{value: %{type: :string, value: pattern}}}, columns) do
    index = Enum.find_index(columns, &(&1 === column))
    regex = to_regex(pattern)
    fn(row) -> Enum.at(row, index + 1) =~ regex end
  end
  defp filter({:ilike, column, %Token{value: %{type: :string, value: pattern}}}, columns) do
    index = Enum.find_index(columns, &(&1 === column))
    regex = to_regex(pattern, [_case_insensitive = "i"])
    fn(row) -> Enum.at(row, index + 1) =~ regex end
  end

  defp to_regex(sql_pattern, options \\ []) do
    options = Enum.join([_unicode = "u" | options])

    sql_pattern
    |> Regex.escape
    |> String.replace("%", ".*")
    |> String.replace("_", ".")
    |> anchor()
    |> Regex.compile!(options)
  end

  defp anchor(pattern), do: "^#{pattern}$"

  defp user_ids(rows), do: Enum.map(rows, &user_id/1)

  defp user_id(row), do: hd(row)
end
