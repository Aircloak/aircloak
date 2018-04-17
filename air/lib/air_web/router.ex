defmodule AirWeb.Router do
  @moduledoc false
  use Air.Web, :router

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
  end

  pipeline :api do
    plug(:accepts, ["json"])
    plug(AirWeb.Plug.Session.ApiAuth, access: :api)
  end

  pipeline :anonymous_only do
    plug(AirWeb.Plug.Session.Anonymous)
  end

  pipeline :browser_auth do
    plug(AirWeb.Plug.Session.Authenticated)
  end

  pipeline :license_validations_browser do
    plug(AirWeb.Plug.ValidateLicense.Browser)
  end

  pipeline :privacy_policy_validations_browser do
    plug(AirWeb.Plug.ValidatePrivacyPolicy.Existence.Browser)
    plug(AirWeb.Plug.ValidatePrivacyPolicy.Acceptance)
  end

  pipeline :policy_validations_api do
    plug(AirWeb.Plug.ValidateLicense.API)
    plug(AirWeb.Plug.ValidatePrivacyPolicy.Existence.API)
    plug(AirWeb.Plug.ValidatePrivacyPolicy.Acceptance)
  end

  scope "/auth", AirWeb do
    # Use the default browser stack
    pipe_through([:browser, :anonymous_only])

    get("/", SessionController, :new)
    post("/", SessionController, :create)
  end

  scope "/", AirWeb, private: %{context: :http} do
    pipe_through([:browser, :browser_auth, :license_validations_browser, :privacy_policy_validations_browser])

    get("/", DataSourceController, :redirect_to_last_used)

    post("/queries", QueryController, :create)
    post("/queries/:id/cancel", QueryController, :cancel)
    get("/queries/load_history/:data_source_name", QueryController, :load_history)
    get("/queries/:id", QueryController, :show)
    get("/queries/:id/buckets", QueryController, :buckets)
    get("/queries/:id/debug_export", QueryController, :debug_export)

    delete("/logout", SessionController, :delete)

    get("/help_guides", HelpGuideController, :index)
    get("/help_guide/:article", HelpGuideController, :article)

    resources("/api_tokens", ApiTokenController)

    resources "/data_sources", DataSourceController do
      resources("/views", ViewController)
    end

    get("/licenses", LicenseController, :index)
    get("/licenses/:realm/:name", LicenseController, :show)
    get("/licenses/dependencies.zip", LicenseController, :dependencies)

    resources("/profile", ProfileController, singleton: true, only: [:edit, :update])
    put("/profile/change_password", ProfileController, :change_password)
    post("/profile/toggle_debug_mode", ProfileController, :toggle_debug_mode)

    get("/changelog", ChangelogController, :index)

    get("/privacy_policy", PrivacyPolicyController, :index)
    put("/privacy_policy/accept/:id", PrivacyPolicyController, :accept)
    put("/privacy_policy/reject/:id", PrivacyPolicyController, :reject)
  end

  scope "/admin", AirWeb.Admin, as: :admin do
    pipe_through([:browser, :browser_auth, :privacy_policy_validations_browser])

    get("/queries/failed", QueryController, :failed)
    get("/queries/:id", QueryController, :show)

    resources("/users", UserController)
    resources("/groups", GroupController)
    resources("/data_sources", DataSourceController)
    resources("/settings", SettingsController, singleton: true)

    get("/audit_log", AuditLogController, :index)
    get("/audit_log/confirm_deletion", AuditLogController, :confirm_deletion)
    post("/audit_log/clear", AuditLogController, :delete_all)

    resources("/cloaks", CloaksController)
    get("/activity_monitor", ActivityMonitorController, :index)
    get("/", WarningsController, :warnings_if_any, as: :warnings_if_any)

    get("/central/export_for_aircloak", CentralController, :export)
    post("/central/new_export", CentralController, :new_export)
    get("/central/download_export/:export_id", CentralController, :download_export)

    get("/warnings", WarningsController, :index)

    resources("/license", LicenseController, only: [:edit, :update], singleton: true)
    resources("/privacy_policy", PrivacyPolicyController)
  end

  scope "/onboarding", AirWeb.Onboarding, as: :onboarding do
    pipe_through([:browser, :anonymous_only])

    get("/", UserController, :new)
    post("/", UserController, :create)
    get("/already_setup", UserController, :already_setup)
  end

  scope "/api", private: %{context: :api} do
    pipe_through([:api, :policy_validations_api])

    resources("/queries", AirWeb.QueryController, only: [:create, :show])
    post("/queries/:id/cancel", AirWeb.QueryController, :cancel)
    resources("/data_sources", AirWeb.API.DataSourceController)
  end
end
