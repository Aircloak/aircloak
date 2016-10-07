defmodule Cloak.Query.LCFConditions do
  @moduledoc """
  Implements handling of filters that should not apply if there are not enough users present.
  This includes negated LIKE, ILIKE and equality WHERE clauses, as well as the IN-clause.
  These need special handling, because a malicious analyst would be able to find out information about
  individuals by adding a condition that would exclude an individual from a result set. Then by comparing
  the result of a query with and without that condition the analyst can find out if that user was in fact
  included in the result set. To avoid this we ignore the condition if it would remove too few users.
  """

  alias Cloak.Query.Anonymizer
  alias Cloak.Aql.{Column, Function, Query}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Applies or ignores potentially low-count filtered conditions in the query to the given rows.
  The input is wrapped in our custom stream object and filtered during processing.
  Note: the order of the input rows is not guaranteed to be kept after filtering.
  """
  @spec apply(Enumerable.t, Query.t) :: Enumerable.t
  def apply(rows, %Query{lcf_check_conditions: []}),
    # no negative conditions, so we immediately pass all the rows through to avoid
    # needless intermediate wrapping which will return all rows anyway
    do: rows
  def apply(rows, %Query{lcf_check_conditions: clauses}) do
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
    # Upon reaching the `hard_limit` number of users the low count threshold is satisfied. We can then
    # either immediately remove rows from the stream (if the `match_decision` is set to `:drop`), or
    # immediately let them pass on for further processing (if the `match_decision` is set to `:keep`).
    # When the end of the stream is reached we also make a final decision based on the `match_decision`
    # and whether or not the `hard_limit` was reached.
    defstruct matcher: nil, matched_rows: [], matched_users: MapSet.new(), match_hard_limit: 0, match_decision: nil

    # Returns true if a filter matches a row.
    def matches?(%Filter{matcher: matcher}, row), do: matcher.(row)

    # At the end of the stream we need to make a decision about the matched rows.
    @dialyzer [:no_opaque, :no_return] # needed because of the Anonymizer.new(MapSet.t) call
    def flush(%Filter{matched_users: users, matched_rows: rows, match_hard_limit: hard_limit,
        match_decision: match_decision}) do
      matched_users_count = MapSet.size(users)
      {sufficient_matches, _} = Anonymizer.sufficiently_large?(Anonymizer.new(users), matched_users_count)
      case matched_users_count >= hard_limit or sufficient_matches do
        true -> flush_action(match_decision, :sufficient_matches, rows)
        false -> flush_action(match_decision, :insufficient_matches, rows)
      end
    end

    defp user_id([user_id | _rest]), do: user_id

    # This is called when a row is matched by the filter.
    # If the number of distinct users for whom we have collected rows does not exceed the hard limit, we store the
    # row in the filter. This allows us to make a final keep or throw decision once the threshold has been reached.
    # If the number of distinct users exceeds the limit, we either drop or keep the row, based on the `match_decision`
    # filter criteria.
    def take(%Filter{matched_rows: rows, matched_users: users, match_hard_limit: hard_limit} = filter, row) do
      matched_users_count = MapSet.size(users)
      case matched_users_count >= hard_limit do
        true -> apply_match_decision(row, filter)
        false ->
          filter_with_row = %Filter{filter | matched_rows: [row | rows],
            matched_users: MapSet.put(users, user_id(row))}
          {filter_with_row, :drop}
      end
    end

    # This is called when:
    # - a row matches a filter, and
    # - there are enough distinct users with the same row to pass the low count filter
    # Depending on the `match_decision` criteria, the row is either kept or dropped from the stream.
    defp apply_match_decision(row, %Filter{match_decision: :keep} = filter), do: {filter, row}
    defp apply_match_decision(_row, %Filter{match_decision: :drop} = filter), do: {filter, :drop}

    # This is called when we have reached the end of the stream, and decides how to handle rows cached in a filter.
    # If the `match_decision` criteria is to:
    # - `keep` and there were sufficient users, then the cached rows are passed on for further processing since
    #   the low count filter was satisfied,
    # - `drop` and there _were not_ sufficient users to reach the low count filter threshold, then the cached
    #   rows are also passed on for further processing, since the negative condition was not satisfied,
    # - otherwise, the rows are dropped
    @dialyzer [:no_unused] # needed because Dialyzer things the parent won't be called
    defp flush_action(:keep, :sufficient_matches, rows), do: rows
    defp flush_action(:drop, :insufficient_matches, rows), do: rows
    defp flush_action(_decision, _whether_sufficient_matches, _rows), do: []
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
      true -> Filter.take(filter, row)
    end
  end

  # Converts the lcf check conditions into filters for the stream of rows.
  defp filters(clauses) do
    {low_count_mean, low_count_sd} = Anonymizer.config(:low_count_soft_lower_bound)
    # Once we match this amount of users we can confidently make a decision to apply the filter.
    hard_limit = low_count_sd * 5 + low_count_mean
    clauses
    |> Enum.flat_map(fn(clause) ->
      for {matcher, match_decision} <- matchers(clause), do:
        %Filter{matcher: matcher, match_hard_limit: hard_limit, match_decision: match_decision}
    end)
    |> Enum.sort_by(&(&1.match_decision))
  end

  defp matchers({:not, {:comparison, column, :=, value}}) do
    value = extract_value(value)
    [{fn (row) -> Function.apply_to_db_row(column, row) == value end, :drop}]
  end
  defp matchers({:not, {:like, column, %Column{type: :text, value: pattern}}}) do
    regex = to_regex(pattern)
    [{fn (row) -> Function.apply_to_db_row(column, row) =~ regex end, :drop}]
  end
  defp matchers({:not, {:ilike, column, %Column{type: :text, value: pattern}}}) do
    regex = to_regex(pattern, [_case_insensitive = "i"])
    [{fn (row) -> Function.apply_to_db_row(column, row) =~ regex end, :drop}]
  end
  defp matchers({:not, {:in, column, values}}) do
    Enum.flat_map(values, &matchers({:not, {:comparison, column, :=, &1}}))
  end
  defp matchers({:in, column, values}) do
    values
    |> Enum.flat_map(&matchers({:not, {:comparison, column, :=, &1}}))
    |> Enum.map(fn({matcher, _action}) -> {matcher, :keep} end)
  end

  defp extract_value(%Column{value: value}), do: value
  defp extract_value(value), do: value

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
