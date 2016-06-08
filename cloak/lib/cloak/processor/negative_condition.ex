defmodule Cloak.Processor.NegativeCondition do
  alias Cloak.Query.Columns
  alias Cloak.SqlQuery.Parsers.Token

  def apply(rows, %{where_not: clauses} = query) do
    clauses
    |> Enum.filter(&sufficient_matches?(&1, rows, query))
    |> Enum.reduce(rows, fn(clause, rows) -> Enum.reject(rows, filter(clause, query)) end)
  end

  def drop_filter_columns(rows, %{filter_columns: filter_columns} = query) do
    anonymizable = Enum.count(Columns.all(query, user_id: true)) - Enum.count(filter_columns)
    Enum.map(rows, &Enum.take(&1, anonymizable))
  end

  defp sufficient_matches?(clause, rows, query) do
    random_seed = rows
    |> Enum.map(&hd/1)
    |> Enum.sort()
    |> :erlang.term_to_binary()
    |> random_seed()

    matches = rows
    |> Enum.filter(filter(clause, query))
    |> Enum.count()

    noisy_matches = :cloak_distributions.gauss_s(sigma_soft_lower_bound, matches, random_seed)

    matches >= absolute_limit && noisy_matches >= soft_limit
  end

  defp filter({:comparison, column, :=, %Token{value: %{value: value}}}, query) do
    index = Columns.index(column, query, user_id: true)
    fn(row) -> Enum.at(row, index) == value end
  end
  defp filter({:like, column, %Token{value: %{type: :string, value: pattern}}}, query) do
    index = Columns.index(column, query, user_id: true)
    regex = to_regex(pattern)
    fn(row) -> Enum.at(row, index) =~ regex end
  end
  defp filter({:ilike, column, %Token{value: %{type: :string, value: pattern}}}, query) do
    index = Columns.index(column, query, user_id: true)
    regex = to_regex(pattern, [_case_insensitive = "i"])
    fn(row) -> Enum.at(row, index) =~ regex end
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

  defp random_seed(binary) do
    <<a::32, b::32, c::64>> = :crypto.hash(:md4, binary)
    {a, b, c}
  end

  defp absolute_limit, do: noise_config(:absolute_lower_bound)

  defp soft_limit, do: noise_config(:soft_lower_bound)

  defp sigma_soft_lower_bound, do: noise_config(:sigma_soft_lower_bound)

  defp noise_config(name), do: Application.get_env(:cloak, :noise) |> Keyword.get(name)
end
