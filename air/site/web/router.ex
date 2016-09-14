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

  pipeline :api do
    plug :accepts, ["json"]
    plug Air.Plug.Session.ApiAuth
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

    get "/", QueryController, :index
    post "/queries", QueryController, :create
    get "/queries/load_history/:data_source_id", QueryController, :load_history
    get "/queries/failed", QueryController, :failed
    get "/queries/:id", QueryController, :show

    delete "/logout", SessionController, :delete

    get "/help_guides", HelpGuideController, :index
    get "/help_guide/:article", HelpGuideController, :article

    resources "/api_tokens", ApiTokenController
    resources "/cloaks", CloaksController
    resources "/organisations", OrganisationController
    resources "/users", UserController

    get "/audit_log/load_entries", AuditLogController, :load_entries
    get "/audit_log", AuditLogController, :index

    resources "/data_sources", DataSourceController

    get "/licenses", LicenseController, :index
    get "/licenses/:realm/:name", LicenseController, :show
  end

  scope "/api" do
    pipe_through [:api]

    resources "/queries", Air.QueryController
    resources "/data_sources", Air.API.DataSourceController
  end
end
