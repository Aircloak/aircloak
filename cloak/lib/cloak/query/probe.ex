defmodule Cloak.Query.Probe do
  @moduledoc """
  Implements modifying the query if certain aspects apply to too few users. Currently it checks:

  * Negative clauses (<>, NOT LIKE) that exclude too few users
  * IN constants that match too few users

  If a given clause or IN constant is found to apply to a low-count number of users it is removed from the query.
  """

  alias Cloak.Sql.{Query, Condition, NoiseLayer}
  alias Cloak.Query.{Anonymizer, DataDecoder, DbEmulator}
  alias Cloak.DataSource
  require Logger

  use Lens.Macros

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
    |> Enum.reduce(query, &clean_lcf_conditions/2)
    |> Query.debug_log("Final query")
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp clean_lcf_conditions(subquery_lens, query) do
    probe = prepare_probe(query)

    query
    |> reject_lcf_conditions(probe, subquery_lens)
    |> reject_lcf_in_constants(probe, subquery_lens)
  end

  defp prepare_probe(query) do
    [uid_column | _] = query.db_columns
    %Query{query | limit: @lcf_upper_limit, offset: 0, distinct?: true, subquery?: true,
      columns: [uid_column], db_columns: [uid_column], group_by: [], order_by: [], having: nil}
  end

  defp reject_lcf_conditions(query, probe, subquery_lens) do
    Lens.map(subquery_lens, query, fn (subquery) ->
      lcf_conditions =
        conditions()
        |> Lens.satisfy(&needs_probe?/1)
        |> Lens.to_list(subquery)
        |> Enum.filter(&lcf_condition?(probe, subquery_lens, &1))
      Query.Lenses.db_filter_clauses()
      |> Lens.map(subquery, fn (clause) -> Condition.reject(clause, & &1 in lcf_conditions) end)
    end)
  end

  defp reject_lcf_in_constants(query, probe, subquery_lens) do
    Lens.map(subquery_lens, query, fn (subquery) ->
      subquery
      |> in_conditions()
      |> Enum.reduce(subquery, &reject_lcf_in_constants(&1, &2, subquery_lens, probe))
    end)
  end

  defp reject_lcf_in_constants(condition = {:in, lhs, constants}, subquery, subquery_lens, probe) do
    conditions()
    |> Lens.satisfy(& &1 == condition)
    |> Lens.map(subquery, fn(_) ->
      case Enum.reject(constants, &lcf_in_constant?(probe, subquery_lens, condition, &1)) do
        [] -> Condition.impossible()
        ok_values -> {:in, lhs, ok_values}
      end
    end)
  end

  defp in_conditions(subquery), do:
    conditions() |> Lens.satisfy(&Condition.in?/1) |> Lens.to_list(subquery)

  defp needs_probe?(condition), do:
    Condition.not_equals?(condition) or Condition.not_like?(condition)

  defp lcf_condition?(probe, subquery_lens, condition), do:
    lcf_after_replacement?(probe, subquery_lens, condition, Condition.negate(condition))

  defp lcf_in_constant?(probe, subquery_lens, condition, constant), do:
    lcf_after_replacement?(probe, subquery_lens, condition, {:comparison, Condition.subject(condition), :=, constant})

  defp lcf_after_replacement?(probe, subquery_lens, condition, replacement) do
    noise_layers = NoiseLayer.new_accumulator(probe.noise_layers)
    subquery_lens
    |> conditions()
    |> Lens.satisfy(&condition == &1)
    |> Lens.map(probe, fn (_) -> replacement end)
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

  deflensp conditions(), do:
    Query.Lenses.db_filter_clauses() |> Query.Lenses.conditions()
end
