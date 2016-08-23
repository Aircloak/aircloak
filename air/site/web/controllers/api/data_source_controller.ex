defmodule Air.API.DataSourceController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{DataSource}

  def permissions do
    %{user: :all}
  end

  def index(conn, _params) do
    data_sources = DataSource
    |> Repo.all()
    |> Enum.map(&DataSource.to_map/1)
    json(conn, data_sources)
  end
end
