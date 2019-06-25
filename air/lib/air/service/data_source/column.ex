defmodule Air.Service.DataSource.Column do
  @moduledoc "Contains a couple functions for gathering information about the state of data source columns."

  @doc "Returns true if both the shadow table and isolators have been computed for the column, false otherwise."
  @spec analyzed_successfully?(map()) :: boolean()
  def analyzed_successfully?(column),
    do: isolators_computed?(column) and shadow_computed?(column) and bounds_computed?(column)

  @doc "Returns true if the column has neither been analyzed correctly nor failed yet, false otherwise."
  @spec analysis_pending?(map()) :: boolean()
  def analysis_pending?(column), do: not analyzed_successfully?(column) and not analysis_failed?(column)

  @doc "Returns true if either the shadow table or isolators have failed to compute for the column, false otherwise."
  @spec analysis_failed?(map()) :: boolean()
  def analysis_failed?(column), do: isolators_failed?(column) or shadow_failed?(column) or bounds_failed?(column)

  @doc "Returns true if isolators have been computed for the column, false otherwise."
  @spec isolators_computed?(map()) :: boolean()
  def isolators_computed?(column), do: is_boolean(column["isolated"])

  @doc "Returns true if isolators have failed to compute for the column, false otherwise."
  @spec isolators_failed?(map()) :: boolean()
  def isolators_failed?(column), do: column["isolated"] == "failed"

  @doc "Returns true if the shadow table has been computed for the column, false otherwise."
  @spec shadow_computed?(map()) :: boolean()
  def shadow_computed?(column), do: column["shadow_table"] == "ok"

  @doc "Returns true if the shadow table has failed to compute for the column, false otherwise."
  @spec shadow_failed?(map()) :: boolean()
  def shadow_failed?(column), do: column["shadow_table"] == "failed"

  @doc "Returns true if the bounds have been computed for the column, false otherwise."
  @spec bounds_computed?(map()) :: boolean()
  def bounds_computed?(column), do: column["bounds"] == "ok"

  @doc "Returns true if the bound computation failed for the column, false otherwise."
  @spec bounds_failed?(map()) :: boolean()
  def bounds_failed?(column), do: column["bounds"] == "failed"
end
