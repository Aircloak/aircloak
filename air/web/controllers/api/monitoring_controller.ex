defmodule Air.API.MonitoringController do
  @moduledoc false
  use Air.Web, :controller

  def permissions do
    %{admin: :all}
  end

  def index(conn, _params) do
    json(conn, Air.Service.Monitoring.assemble_info())
  end
end
