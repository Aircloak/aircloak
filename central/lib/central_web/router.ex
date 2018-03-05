defmodule CentralWeb.Router do
  @moduledoc false
  use Central.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :proxy do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :put_secure_browser_headers
  end

  pipeline :browser_auth do
    plug CentralWeb.Plug.Session.Authenticated
  end

  pipeline :browser_anonymous do
    plug CentralWeb.Plug.Session.Anonymous
  end

  scope "/auth", CentralWeb do
    pipe_through [:browser, :browser_anonymous] # Use the default browser stack

    get "/", SessionController, :new
    post "/", SessionController, :create
  end

  scope "/kibana", CentralWeb do
    pipe_through [:proxy, :browser_auth]
    get "/", KibanaProxyController, :redirect_to_web_interface
    get "/*path", KibanaProxyController, :get
    post "/*path", KibanaProxyController, :post
    put "/*path", KibanaProxyController, :put
  end

  scope "/", CentralWeb do
    pipe_through [:browser, :browser_auth]

    resources "/users", UserController
    resources "/customers", CustomerController do
      get "/token", CustomerController, :token, as: :token
      resources "/licenses", LicenseController, only: [:index, :create, :show]
    end
    resources "/stats", StatsController

    get "/", StatsController, :index
    delete "/logout", SessionController, :delete

    resources "/imports", ImportController
  end
end
