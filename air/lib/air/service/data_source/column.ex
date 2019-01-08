defmodule Air.Service.DataSource.Column do
  @moduledoc "Contains a couple functions for gathering information about the state of data source columns."

  @spec analyzed?(map()) :: boolean()
  @doc "Returns true if both the shadow table and isolators have been computed for the column, false otherwise."
  def analyzed?(column), do: isolators_computed?(column) and shadow_computed?(column)

  @spec analysis_failed?(map()) :: boolean()
  @doc "Returns true if either the shadow table or isolators have failed to compute for the column, false otherwise."
  def analysis_failed?(column), do: isolators_failed?(column) or shadow_failed?(column)

  @spec isolators_computed?(map()) :: boolean()
  @doc "Returns true if isolators have been computed for the column, false otherwise."
  def isolators_computed?(column), do: is_boolean(column["isolated"])

  @spec isolators_failed?(map()) :: boolean()
  @doc "Returns true if isolators have failed to compute for the column, false otherwise."
  def isolators_failed?(column), do: column["isolated"] == "failed"

  @spec shadow_computed?(map()) :: boolean()
  @doc "Returns true if the shadow table has been computed for the column, false otherwise."
  def shadow_computed?(column), do: column["shadow_table"] == "ok"

  @spec shadow_failed?(map()) :: boolean()
  @doc "Returns true if the shadow table has failed to compute for the column, false otherwise."
  def shadow_failed?(column), do: column["shadow_table"] == "failed"
end
