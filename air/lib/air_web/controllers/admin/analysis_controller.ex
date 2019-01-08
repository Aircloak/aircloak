defmodule AirWeb.Admin.AnalysisController do
  @moduledoc """
  Controller for administrators to get a view of the state of the analysis queries in the system.
  """

  use Air.Web, :admin_controller

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions(), do: %{admin: :all}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html", groups: tables())
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp tables() do
    for data_source <- Air.Service.DataSource.all(),
        table <- Air.Schemas.DataSource.tables(data_source) do
      %{
        name: "#{data_source.name}/#{table["id"]}",
        columns: length(table["columns"]),
        isolators_computed: Enum.count(table["columns"], &isolators_computed?/1),
        isolators_failed: Enum.count(table["columns"], &isolators_failed?/1),
        rare_values_computed: Enum.count(table["columns"], &shadow_computed?/1),
        rare_values_failed: Enum.count(table["columns"], &shadow_failed?/1),
        analyzed: Enum.count(table["columns"], &analyzed?/1),
        analysis_failed: Enum.count(table["columns"], &analysis_failed?/1)
      }
    end
  end

  defp analyzed?(column), do: isolators_computed?(column) and shadow_computed?(column)

  defp analysis_failed?(column), do: isolators_failed?(column) or shadow_failed?(column)

  defp isolators_computed?(column), do: is_boolean(column["isolated"])

  defp isolators_failed?(column), do: column["isolated"] == "failed"

  defp shadow_computed?(column), do: column["shadow_table"] == "ok"

  defp shadow_failed?(column), do: column["shadow_table"] == "failed"
end
