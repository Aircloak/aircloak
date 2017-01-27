defmodule Air.MonitoringRouter do
  @moduledoc false
  use Air.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
    plug Air.Plug.Session.ApiAuth
  end

  scope "/" do
    pipe_through [:api]

    get "/", Air.API.MonitoringController, :index
  end
end
