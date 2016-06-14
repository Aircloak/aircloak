defmodule Air.API.DataSourceController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{DataSource}

  def permissions do
    %{user: :all}
  end

  def index(conn, _params) do
    json(conn, DataSource.all(conn.assigns.current_user.organisation))
  end
end
