defmodule Cloak.Query.Probe do
  @moduledoc """
  Implements filtering of conditions that cannot be anonymized using noise layers and should not apply
  if there are not enough users present. These are negative conditions that exclude users and need special
  handling, because the excluded users won't be part of the result set. We ignore the condition if it
  aplies to too few users.
  """

  alias Cloak.Sql.{Query, Condition, NoiseLayer}
  alias Cloak.Query.{Anonymizer, DataDecoder, DbEmulator}
  alias Cloak.DataSource
  require Logger


  # The upper limit for the count of users over which a condition is not low-count filtered.
  @lcf_upper_limit 15

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Keeps or rejects conditions in the query depending on the amount of users excluded by them.
  """
  @spec process(Query.t) :: Query.t
  def process(%Query{command: :show} = query), do: query
  def process(%Query{command: :select} = query) do
    Logger.debug("Probing dataset ...")
    query
    |> Query.Lenses.subquery_lenses()
    |> Enum.reduce(query, &reject_lcf_conditions/2)
    |> Query.debug_log("Final query")
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp prepare_probe(query) do
    [uid_column | _] = query.db_columns
    %Query{query | limit: @lcf_upper_limit, offset: 0, distinct?: true, subquery?: true,
      columns: [uid_column], db_columns: [uid_column], group_by: [], order_by: [], having: nil}
  end

  defp reject_lcf_conditions(subquery_lens, query) do
    probe = prepare_probe(query)
    Lens.map(subquery_lens, query, fn (subquery) ->
      lcf_conditions =
        Query.Lenses.db_filter_clauses()
        |> Query.Lenses.conditions()
        |> Lens.satisfy(&needs_probe?/1)
        |> Lens.to_list(subquery)
        |> Enum.filter(&lcf_condition?(probe, subquery_lens, &1))
      Query.Lenses.db_filter_clauses()
      |> Lens.map(subquery, fn (clause) -> Condition.reject(clause, & &1 in lcf_conditions) end)
    end)
  end

  defp needs_probe?(condition), do:
    Condition.not_equals?(condition) or Condition.not_like?(condition)

  defp lcf_condition?(probe, subquery_lens, condition) do
    noise_layers = NoiseLayer.new_accumulator(probe.noise_layers)
    subquery_lens
    |> Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&condition == &1)
    |> Lens.map(probe, &Condition.negate(&1))
    |> Query.debug_log("Executing probe")
    |> select_rows()
    |> List.flatten()
    |> MapSet.new()
    |> insufficient_users?(noise_layers)
  end

  defp select_rows(%Query{emulated?: true} = query), do:
    query |> DbEmulator.select() |> Enum.to_list()
  defp select_rows(%Query{emulated?: false} = query), do:
    DataSource.select!(query, & &1 |> DataDecoder.decode(query) |> Enum.to_list())

  def insufficient_users?(user_ids, noise_layers) do
    user_count = MapSet.size(user_ids)
    {sufficient_users?, _} =
      [user_ids | noise_layers]
      |> Anonymizer.new()
      |> Anonymizer.sufficiently_large?(user_count)
    user_count < @lcf_upper_limit and not sufficient_users?
  end
end
