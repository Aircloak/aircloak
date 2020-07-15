defmodule Air.Service.DataSource.Column do
  @moduledoc "Contains a couple functions for gathering information about the state of data source columns."

  @doc "Returns true if the shadow table, isolators, and bounds have been computed for the column, false otherwise."
  @spec analyzed_successfully?(map()) :: boolean()
  def analyzed_successfully?(column),
    do: isolators_computed?(column) and shadow_computed?(column) and bounds_computed?(column)

  @doc "Returns true if the column is pending any kind of computation, false otherwise."
  @spec analysis_pending?(map()) :: boolean()
  def analysis_pending?(column),
    do: isolators_pending?(column) or shadow_pending?(column) or bounds_pending?(column)

  @doc "Returns true if either the shadow table or isolators have failed to compute for the column, false otherwise."
  @spec analysis_failed?(map()) :: boolean()
  def analysis_failed?(column), do: isolators_failed?(column) or shadow_failed?(column) or bounds_failed?(column)

  @doc "Returns true if isolators have been computed for the column, false otherwise."
  @spec isolators_computed?(map()) :: boolean()
  def isolators_computed?(column), do: is_boolean(column["isolated"])

  @doc "Returns true if isolators have failed to compute for the column, false otherwise."
  @spec isolators_failed?(map()) :: boolean()
  def isolators_failed?(column), do: column["isolated"] == "failed"

  @doc "Returns true if isolators are pending computation for the column, false otherwise."
  @spec isolators_pending?(map()) :: boolean()
  def isolators_pending?(column), do: column["isolated"] == "pending"

  @doc "Returns true if the shadow table has been computed for the column, false otherwise."
  @spec shadow_computed?(map()) :: boolean()
  def shadow_computed?(column), do: column["shadow_table"] == "ok"

  @doc "Returns true if the shadow table has failed to compute for the column, false otherwise."
  @spec shadow_failed?(map()) :: boolean()
  def shadow_failed?(column), do: column["shadow_table"] == "failed"

  @doc "Returns true if the shadow table is pending computation for the column, false otherwise."
  @spec shadow_pending?(map()) :: boolean()
  def shadow_pending?(column), do: column["shadow_table"] == "pending"

  @doc "Returns true if the bounds have been computed for the column, false otherwise."
  @spec bounds_computed?(map()) :: boolean()
  def bounds_computed?(column), do: column["bounds"] == "ok"

  @doc "Returns true if the bound computation failed for the column, false otherwise."
  @spec bounds_failed?(map()) :: boolean()
  def bounds_failed?(column), do: column["bounds"] == "failed"

  @doc "Returns true if the bound computation is pending for the column, false otherwise."
  @spec bounds_pending?(map()) :: boolean()
  def bounds_pending?(column), do: column["bounds"] == "pending"
end
