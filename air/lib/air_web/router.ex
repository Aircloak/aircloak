defmodule AirWeb.Router do
  @moduledoc false
  use Air.Web, :router
  use Plug.ErrorHandler

  import Phoenix.LiveDashboard.Router

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
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
    plug(AirWeb.Plug.Session.Restoration)
    plug(AirWeb.Plug.Session.Authenticated)
  end

  pipeline :browser_for_all do
    plug(AirWeb.Plug.Session.EveryoneAllowed)
  end

  pipeline :admin_only do
    plug(AirWeb.Plug.AdminOnly)
  end

  pipeline :license_validations_browser do
    plug(AirWeb.Plug.ValidateLicense.Browser)
  end

  pipeline :license_validations_api do
    plug(AirWeb.Plug.ValidateLicense.API)
  end

  scope "/auth", AirWeb do
    # Use the default browser stack
    pipe_through([:browser, :anonymous_only])

    get("/", SessionController, :new)
    post("/", SessionController, :create)
  end

  scope "/permalink/public", AirWeb do
    pipe_through([:browser, :browser_for_all])

    get("/query/:token", QueryController, :permalink_show, as: :public_permalink)
    get("/query/:token/buckets", QueryController, :permalink_buckets, as: :public_permalink)
  end

  scope "/permalink/private", AirWeb do
    pipe_through([:browser, :browser_auth])

    get("/query/:token", QueryController, :permalink_show, as: :private_permalink)
    get("/query/:token/buckets", QueryController, :permalink_buckets, as: :private_permalink)
  end

  scope "/reset_password", AirWeb do
    pipe_through([:browser, :anonymous_only])

    resources("/", ResetPasswordController, singleton: true, only: [:show, :update])
    get("/forgot", ResetPasswordController, :forgot)
  end

  scope "/privacy_policy", AirWeb, private: %{context: :http} do
    pipe_through([:browser, :browser_for_all])
    get("/", PrivacyPolicyController, :index)
  end

  scope "/", AirWeb, private: %{context: :http} do
    pipe_through([:browser, :browser_auth, :license_validations_browser])

    get("/", DataSourceController, :redirect_to_last_used)

    resources("/queries", QueryController, only: [:create, :show, :delete])
    post("/queries/:id/cancel", QueryController, :cancel)
    get("/queries/load_history/:data_source_name", QueryController, :load_history)
    get("/queries/:id/buckets", QueryController, :buckets)
    get("/queries/:id/debug_export", QueryController, :debug_export)

    delete("/logout", SessionController, :delete)

    resources("/api_tokens", ApiTokenController)
    resources("/app_logins", AppLoginController)

    resources "/data_sources", DataSourceController do
      get("/selectables/edit", SelectableController, :edit)
      resources("/selectables/:kind", SelectableController)
    end

    get("/licenses", LicenseController, :index)
    get("/licenses/:realm/:name", LicenseController, :show)
    get("/licenses/dependencies.zip", LicenseController, :dependencies)

    scope "/settings" do
      get("/privacy", SettingsController, :privacy)

      get("/security", SettingsController, :security)
      put("/security", SettingsController, :change_password)

      get("/", SettingsController, :profile)
      put("/", SettingsController, :update)
      patch("/toggle_debug_mode", SettingsController, :toggle_debug_mode)

      delete("/sessions", SettingsController, :delete_sessions)
    end

    get("/export", ExportsController, :show)

    get("/changelog", ChangelogController, :index)
  end

  scope "/docs", AirWeb do
    pipe_through([:browser, :browser_for_all])

    get("/", DocsController, :index)
    get("/_sidebar.md", DocsController, :sidebar)
    get("/diffix.md", DocsController, :diffix_page)
    get("/attacks.md", DocsController, :attacks_page)
    get("/*other", DocsController, :redirect)
  end

  scope "/admin", AirWeb.Admin, as: :admin do
    pipe_through([:browser, :browser_auth, :admin_only])

    delete("/users/delete_disabled", UserController, :delete_disabled)

    resources("/users", UserController) do
      put("/enable", UserController, :enable)
      put("/disable", UserController, :disable)
      get("/reset_password", UserController, :reset_password)
      delete("/sessions", UserController, :delete_sessions)
    end

    post("/users/sync_ldap", UserController, :sync_ldap)

    resources("/groups", GroupController)
    post("/groups/sync_ldap", GroupController, :sync_ldap)

    resources("/data_sources", DataSourceController)
    get("/data_sources/:id/analyst_tables/:table_id", DataSourceController, :show_analyst_table)
    post("/data_sources/:id/analyst_tables/:table_id/to_view", DataSourceController, :convert_table_to_view)

    resources("/analysis", AnalysisController)
    resources("/settings", SettingsController, singleton: true)

    get("/audit_log/confirm_deletion", AuditLogController, :confirm_deletion)
    post("/audit_log/clear", AuditLogController, :delete_all)
    live("/audit_log", AuditLogLive.Index, :index, layout: {AirWeb.LayoutView, :admin})
    live("/queries/performance", QueryPerformanceLive.Index, :index, layout: {AirWeb.LayoutView, :admin})

    get("/queries/failed", QueryController, :failed)
    get("/queries/:id", QueryController, :show)
    resources("/cloaks", CloaksController)
    post("/cloaks/:id/reinitialize", CloaksController, :reinitialize)
    get("/activity_monitor", ActivityMonitorController, :index)
    get("/", WarningsController, :warnings_if_any, as: :warnings_if_any)

    get("/warnings", WarningsController, :index)

    resources("/license", LicenseController, only: [:edit, :update], singleton: true)
    resources("/privacy_policy", PrivacyPolicyController)
    resources("/diffix-explorer", ExplorerController, only: [:index, :show])
    live_dashboard("/live-dashboard", metrics: AirWeb.Telemetry)
  end

  scope "/onboarding", AirWeb.Onboarding, as: :onboarding do
    pipe_through([:browser, :anonymous_only])

    get("/", UserController, :new)
    post("/", UserController, :create)
    get("/already_setup", UserController, :already_setup)
  end

  scope "/api", private: %{context: :api} do
    pipe_through([:api, :license_validations_api])

    resources("/queries", AirWeb.QueryController, only: [:create, :show])
    post("/queries/:id/cancel", AirWeb.QueryController, :cancel)
    resources("/data_sources", AirWeb.API.DataSourceController)
  end
end
