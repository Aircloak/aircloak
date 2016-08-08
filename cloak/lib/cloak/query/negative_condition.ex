defmodule Cloak.Query.NegativeCondition do
  @moduledoc """
  Implements handling of negated LIKE, ILIKE and equality WHERE clauses on the side of the application.
  These need special handling, because a malicious analyst would be able to find out information about
  individuals by adding a condition that would exclude an individual from a result set. Then by comparing
  the result of a query with and without that condition the analyst can find out if that user was in fact
  included in the result set. To avoid this we ignore the condition if it would remove too few users.
  """

  alias Cloak.Query.Anonymizer
  alias Cloak.Aql.Query
  alias Cloak.SqlQuery.Function
  alias Cloak.SqlQuery.Parsers.Token


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Applies or ignores negative conditions in the query to the given rows.
  The input is wrapped in our custom stream object and filtered during processing.
  Note: the order of the input rows is not guaranteed to be kept after filtering.
  """
  @spec apply(Enumerable.t, Query.t) :: Enumerable.t
  def apply(rows, %Query{where_not: []}),
    # no negative conditions, so we immediately pass all the rows through to avoid
    # needless intermediate wrapping which will return all rows anyway
    do: rows
  def apply(rows, %Query{where_not: clauses}) do
    rows
    # add one more element so we can produce additional rows after the input has been exhausted
    |> Stream.concat([:done])
    |> Stream.transform(filters(clauses), &process_input_row/2)
  end


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

    # At the end of the stream we need to make a decision about the matched rows.
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

  defp process_input_row(:done, filters), do:
    {Enum.flat_map(filters, &Filter.flush/1), nil}
  defp process_input_row(row, filters) do
    case Enum.map_reduce(filters, row, &match_filter/2) do
      {filters, :drop} ->
        {[], filters}

      {filters, _row} ->
        {[row], filters}
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

  # Converts the 'where not' clauses into filters for the stream of rows.
  defp filters(clauses) do
    {low_count_mean, low_count_sd} = Anonymizer.config(:low_count_soft_lower_bound)
    # Once we match this amount of users we can confidently make a decision to apply the filter.
    hard_limit = low_count_sd * 5 + low_count_mean
    for clause <- clauses, do: %Filter{matcher: matcher(clause), match_hard_limit: hard_limit}
  end

  defp matcher({:comparison, column, :=, %Token{value: %{value: value}}}) do
    fn (row) -> Function.apply_to_db_row(column, row) == value end
  end
  defp matcher({:comparison, column, :=, value}) do
    fn (row) -> Function.apply_to_db_row(column, row) == value end
  end
  defp matcher({:like, column, %Token{value: %{type: :string, value: pattern}}}) do
    regex = to_regex(pattern)
    fn (row) -> Function.apply_to_db_row(column, row) =~ regex end
  end
  defp matcher({:ilike, column, %Token{value: %{type: :string, value: pattern}}}) do
    regex = to_regex(pattern, [_case_insensitive = "i"])
    fn (row) -> Function.apply_to_db_row(column, row) =~ regex end
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
