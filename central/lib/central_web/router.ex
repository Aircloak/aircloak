defmodule CentralWeb.Router do
  @moduledoc false
  use Central.Web, :router

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
  end

  pipeline :proxy do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(:put_secure_browser_headers)
  end

  pipeline :browser_auth do
    plug(CentralWeb.Plug.Session.Authenticated)
  end

  pipeline :browser_anonymous do
    plug(CentralWeb.Plug.Session.Anonymous)
  end

  scope "/auth", CentralWeb do
    # Use the default browser stack
    pipe_through([:browser, :browser_anonymous])

    get("/", SessionController, :new)
    post("/", SessionController, :create)
  end

  scope "/", CentralWeb do
    pipe_through([:browser, :browser_auth])

    resources("/users", UserController)

    resources "/customers", CustomerController do
      get("/token", CustomerController, :token, as: :token)

      resources "/licenses", LicenseController, only: [:index, :create, :show, :edit, :update] do
        put("/revoke", LicenseController, :revoke, as: :revoke)
        put("/restore", LicenseController, :restore, as: :restore)
      end
    end

    resources("/stats", StatsController)

    get("/", CustomerController, :index)
    delete("/logout", SessionController, :delete)

    resources("/imports", ImportController)
  end
end
