defmodule AirWeb.MonitoringRouter do
  @moduledoc false
  use Air.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
    plug AirWeb.Plug.Session.ApiAuth, access: :monitoring
  end

  scope "/" do
    pipe_through [:api]

    get "/", Air.API.MonitoringController, :index
  end
end
