defmodule Central.Router do
  @moduledoc false
  use Central.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :browser_auth do
    plug Central.Plug.Session.Authenticated
  end

  pipeline :browser_anonymous do
    plug Central.Plug.Session.Anonymous
  end

  scope "/auth", Central do
    pipe_through [:browser, :browser_anonymous] # Use the default browser stack

    get "/", SessionController, :new
    post "/", SessionController, :create
  end

  scope "/", Central do
    pipe_through [:browser, :browser_auth]
    resources "/users", UserController
    resources "/customers", CustomerController do
      get "/token", CustomerController, :token, as: :token
    end
    resources "/stats", StatsController

    get "/", StatsController, :index
    delete "/logout", SessionController, :delete
  end
end
