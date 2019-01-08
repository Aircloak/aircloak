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
    render(conn, "index.html", by_table: tables, by_data_source: group(tables, & &1.data_source), by_host: tables)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @stats ~w(
    columns isolators_computed isolators_failed rare_values_computed rare_values_failed analyzed analysis_failed)a

  defp group(tables, fun) do
    tables
    |> Enum.group_by(fun)
    |> Enum.map(fn {name, tables} ->
      initial = Enum.reduce(@stats, %{name: name}, &Map.put(&2, &1, 0))

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
