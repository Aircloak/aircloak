defmodule Air.API.DataSourceController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Service.DataSource

  def permissions do
    %{user: :all}
  end

  def index(conn, _params) do
    data_sources = DataSource.all()
    |> Enum.map(&to_map(conn.assigns.current_user, &1))
    json(conn, data_sources)
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  defp to_map(user, data_source) do
    %{
      id: data_source.id,
      token: data_source.global_id,
      name: data_source.name,
      description: data_source.description,
      tables: DataSource.tables(user, data_source),
      errors: DataSource.errors(data_source),
    }
  end
end
