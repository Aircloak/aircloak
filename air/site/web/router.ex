defmodule Air.Router do
  @moduledoc false
  use Air.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :anonymous_only do
    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.EnsureNotAuthenticated, handler: Air.SessionController
  end

  pipeline :browser_auth do
    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.EnsureAuthenticated, handler: Air.SessionController
    plug Guardian.Plug.LoadResource
  end

  scope "/auth", Air do
    pipe_through [:browser, :anonymous_only] # Use the default browser stack

    get "/", SessionController, :new
    post "/", SessionController, :create
  end

  scope "/", Air do
    pipe_through [:browser, :browser_auth]

    get "/", PageController, :index
    delete "/logout", SessionController, :delete

    resources "/users", UserController
    resources "/organisations", OrganisationController
    resources "/tasks", TaskController
  end
end
