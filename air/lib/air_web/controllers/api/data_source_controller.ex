defmodule AirWeb.API.DataSourceController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{Service, Schemas}

  def permissions do
    %{user: :all}
  end

  def index(conn, _params) do
    data_sources = Service.DataSource.all()
    |> Enum.map(&to_map(conn.assigns.current_user, &1))
    json(conn, data_sources)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_map(user, data_source) do
    tables = Service.DataSource.views_and_tables(user, data_source)
    |> Enum.map(& Map.delete(&1, :internal_id))

    %{
      id: data_source.id,
      name: data_source.name,
      description: data_source.description,
      tables: tables,
      errors: Schemas.DataSource.errors(data_source),
    }
  end
end
