defmodule AirWeb.Admin.AnalysisController do
  @moduledoc """
  Controller for administrators to get a view of the state of the analysis queries in the system.
  """

  use Air.Web, :admin_controller
  alias Air.Service.DataSource.Column

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
        isolators_computed: Enum.count(table["columns"], &Column.isolators_computed?/1),
        isolators_failed: Enum.count(table["columns"], &Column.isolators_failed?/1),
        rare_values_computed: Enum.count(table["columns"], &Column.shadow_computed?/1),
        rare_values_failed: Enum.count(table["columns"], &Column.shadow_failed?/1),
        analyzed: Enum.count(table["columns"], &Column.analyzed?/1),
        analysis_failed: Enum.count(table["columns"], &Column.analysis_failed?/1)
      }
    end
  end
end
