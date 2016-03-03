defmodule Air.Router do
  use Air.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :browser_auth do
    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.EnsureAuthenticated, handler: Air.SessionController
    plug Guardian.Plug.LoadResource
  end

  scope "/auth", Air do
    pipe_through :browser # Use the default browser stack

    get "/", SessionController, :new
    post "/", SessionController, :create
    get "/logout", SessionController, :delete
  end

  scope "/", Air do
    pipe_through [:browser, :browser_auth]

    get "/", PageController, :index
    resources "/users", UserController
  end
end
