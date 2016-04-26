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
    plug Air.Plug.Session.Anonymous
  end

  pipeline :browser_auth do
    plug Air.Plug.Session.Authenticated
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

    resources "/cloaks", CloaksController
    resources "/users", UserController
    resources "/organisations", OrganisationController
    resources "/tasks", TaskController, except: [:show, :create]
    post "/tasks/:id/run", TaskController, :run_task
  end
end
