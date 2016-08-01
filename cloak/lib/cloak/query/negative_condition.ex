defmodule Cloak.Query.NegativeCondition do
  @moduledoc """
  Implements handling of negated LIKE, ILIKE and equality WHERE clauses on the side of the application.
  These need special handling, because a malicious analyst would be able to find out information about
  individuals by adding a condition that would exclude an individual from a result set. Then by comparing
  the result of a query with and without that condition the analyst can find out if that user was in fact
  included in the result set. To avoid this we ignore the condition if it would remove too few users.
  The module creates a wrapper stream for the incoming collection of rows that applies the filtering
  conditions when the stream is processed.
  """

  alias Cloak.DataSource
  alias Cloak.Query.Anonymizer
  alias Cloak.SqlQuery
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token

  defmodule Stream do
    @moduledoc """
    This struct acts as a wrapper for the original collection of rows.
    We implement the Enumerable protocol for it and filter the rows during the reduce callback.
    """
    defstruct rows: [], filters: []

    defimpl Enumerable, for: Stream do
      def count(_), do: {:error, __MODULE__}

      def member?(_, _), do: {:error, __MODULE__}

      def reduce(%Stream{rows: rows, filters: []}, acc, fun),
        do: Enumerable.reduce(rows, acc, fun)
      def reduce(%Stream{rows: rows, filters: filters}, acc, fun),
        do: Cloak.Query.NegativeCondition.reduce(rows, filters, acc, fun)
    end
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Applies or ignores negative conditions in the query to the given rows.
  The input is wrapped in our custom stream object and filtered during processing.
  Note: the order of the input rows is not guaranteed to be kept after filtering.
  """
  @spec apply(Enumerable.t, [DataSource.column], Parser.compiled_query) :: Enumerable.t
  def apply(rows, columns, %{where_not: clauses}),
    do: %Stream{rows: rows, filters: filters(columns, clauses)}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defmodule Filter do
    @moduledoc false
    # This holds the state for a filter during the stream processing.
    # We capture all matched rows and users until we are have enough information to make a decision.
    # If we hit the `hard_limit` amount of users, we stop gathering and start dropping rows directly.
    # Otherwise, we make a decision when the end of the stream is reached.
    defstruct matcher: nil, matched_rows: [], matched_users: MapSet.new(), match_hard_limit: 0

    # Returns true if a filter matches a row.
    def matches?(%Filter{matcher: matcher}, row), do: matcher.(row)

    # At the end of the stream we need to make a decission about the matched rows.
    # If we have enough matched users, we drop any rows captured,
    # otherwise we send them forward for processing.
    @dialyzer [:no_opaque, :no_return] # needed becaus of the Anonymizer.new(MapSet.t) call
    def flush(%Filter{matched_users: users, matched_rows: rows, match_hard_limit: hard_limit}) do
      matched_users_count = MapSet.size(users)
      {sufficient_matches, _} = Anonymizer.sufficiently_large?(Anonymizer.new(users), matched_users_count)
      case matched_users_count >= hard_limit or sufficient_matches do
        true -> []
        false -> rows
      end
    end

    defp user_id([user_id | _rest]), do: user_id

    # This is called when a row is matched by the filter.
    # If we are under the hard limit of matched users, we store the row for later processing.
    # otherwise, we can drop the row right now and not worry about it anymore.
    def take(%Filter{matched_rows: rows, matched_users: users, match_hard_limit: hard_limit} = filter, row) do
      matched_users_count = MapSet.size(users)
      case matched_users_count >= hard_limit do
        true -> filter
        false -> %Filter{filter | matched_rows: [row | rows], matched_users: MapSet.put(users, user_id(row))}
      end
    end
  end

  # Checks to see if a filter matches a row.
  # On match, we ask the filter to take the row and we instruct the caller to drop the row
  # from processing for now. The row might still get processed later during the flush step.
  defp match_filter(filter, :drop), do: {filter, :drop}
  defp match_filter(filter, row) do
    case Filter.matches?(filter, row) do
      false -> {filter, row}
      true -> {Filter.take(filter, row), :drop}
    end
  end

  # This is a wrapper for the original reducer function.
  # For each row, we ask the filters for a decision.
  # If we get a match, we drop the row, otherwise we send it forward for processing.
  defp reducer(row, {filters, acc, fun}) do
    case Enum.map_reduce(filters, row, &match_filter/2) do
      {filters, :drop} ->
        {:cont, {filters, acc, fun}}
      {filters, _row} ->
        {state, acc} = fun.(row, acc)
        {state, {filters, acc, fun}}
    end
  end

  # At the end of the stream, we collect any remaining rows and send them forward for prcessing.
  defp flush_filters(filters, acc, fun) do
    filters
    |> Enum.flat_map(&Filter.flush/1)
    |> Enumerable.reduce({:cont, acc}, fun)
  end

  # This is the called during stream processing when we have active filters.
  # We wrap the reducer function and iterate through the rows, applying the filters for each row.
  # It needs to be public because it is called from the `Enumerable` protocol implementation.
  @doc false
  def reduce(rows, filters, {:cont, acc}, fun) do
    case Enumerable.reduce(rows, {:cont, {filters, acc, fun}}, &reducer/2) do
      {:done, {filters, acc, _fun}} -> flush_filters(filters, acc, fun)
      {:halted, {_filters, acc, _fun}} -> {:halted, acc}
    end
  end

  # Converts the 'where not' clauses into filters for the stream of rows.
  defp filters(columns, clauses) do
    {low_count_mean, low_count_sd} = :cloak_conf.get_val(:anonymizer, :low_count_soft_lower_bound)
    # Once we match this amount of users we can confidently make a decision to apply the filter.
    hard_limit = low_count_sd * 5 + low_count_mean
    for clause <- clauses, do: %Filter{matcher: matcher(columns, clause), match_hard_limit: hard_limit}
  end

  def column_to_index(column, columns) do
    column = SqlQuery.full_column_name(column)
    case Enum.find_index(columns, &(&1 === column)) do
      nil -> raise(Cloak.Query.Runner.RuntimeError, "Column `#{column}` doesn't exist in selected columns.")
      index -> index
    end
  end

  defp matcher(columns, {:comparison, column, :=, %Token{value: %{value: value}}}) do
    index = column_to_index(column, columns)
    fn (row) -> Enum.at(row, index) == value end
  end
  defp matcher(columns, {:comparison, column, :=, value}) do
    index = column_to_index(column, columns)
    fn (row) -> Enum.at(row, index) == value end
  end
  defp matcher(columns, {:like, column, %Token{value: %{type: :string, value: pattern}}}) do
    regex = to_regex(pattern)
    index = column_to_index(column, columns)
    fn (row) -> Enum.at(row, index) =~ regex end
  end
  defp matcher(columns, {:ilike, column, %Token{value: %{type: :string, value: pattern}}}) do
    regex = to_regex(pattern, [_case_insensitive = "i"])
    index = column_to_index(column, columns)
    fn (row) -> Enum.at(row, index) =~ regex end
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
end
