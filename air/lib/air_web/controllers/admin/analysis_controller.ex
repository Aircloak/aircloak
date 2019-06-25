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
    tables = tables()

    render(conn, "index.html",
      full_width: true,
      by_table: group(tables, & &1.name),
      by_data_source: group(tables, & &1.data_source),
      by_host: group(tables, & &1.host)
    )
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @stats ~w(
    columns isolators_computed isolators_failed rare_values_computed rare_values_failed analyzed_successfully
    analysis_failed analysis_pending bounds_computed bounds_failed
  )a

  defp group(tables, fun) do
    tables
    |> Enum.group_by(fun)
    |> Enum.map(fn {name, tables} ->
      initial = %{name: name, data_source: hd(tables).data_source}
      initial = Enum.reduce(@stats, initial, &Map.put(&2, &1, 0))

      Enum.reduce(tables, initial, fn table, acc ->
        Enum.reduce(@stats, acc, fn key, acc ->
          Map.put(acc, key, acc[key] + table[key])
        end)
      end)
    end)
  end

  defp tables() do
    for data_source <- Air.Service.DataSource.all(), table <- Air.Schemas.DataSource.tables(data_source) do
      %{
        name: "#{data_source.name}/#{table["id"]}",
        data_source: data_source.name,
        host: data_source.database_host,
        columns: length(table["columns"]),
        analyzed_successfully: Enum.count(table["columns"], &Column.analyzed_successfully?/1),
        analysis_pending: Enum.count(table["columns"], &Column.analysis_pending?/1),
        analysis_failed: Enum.count(table["columns"], &Column.analysis_failed?/1),
        isolators_computed: Enum.count(table["columns"], &Column.isolators_computed?/1),
        isolators_failed: Enum.count(table["columns"], &Column.isolators_failed?/1),
        rare_values_computed: Enum.count(table["columns"], &Column.shadow_computed?/1),
        rare_values_failed: Enum.count(table["columns"], &Column.shadow_failed?/1),
        bounds_computed: Enum.count(table["columns"], &Column.bounds_computed?/1),
        bounds_failed: Enum.count(table["columns"], &Column.bounds_failed?/1)
      }
    end
  end
end
