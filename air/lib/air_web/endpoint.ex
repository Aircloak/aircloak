defmodule AirWeb.Endpoint do
  @moduledoc "Implements the HTTP server for insights.aircloak.com."

  use Phoenix.Endpoint, otp_app: :air

  @session_options [
    store: :cookie,
    secure: false,
    key: "_air_key",
    signing_salt: {Air.Service.Salts, :get, [:session_signing]},
    encryption_salt: {Air.Service.Salts, :get, [:session_encryption]},
    extra: "SameSite=Strict"
  ]

  # -------------------------------------------------------------------
  # Endpoint HTTP specification
  # -------------------------------------------------------------------

  socket("/cloak/socket", AirWeb.Socket.Cloak, websocket: [serializer: [{AirWeb.Socket.Cloak.Serializer, "~> 2.0.0"}]])
  socket("/frontend/socket", AirWeb.Socket.Frontend, websocket: true, longpoll: true)

  socket("/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]],
    longpoll: [connect_info: [session: @session_options]]
  )

  plug(
    AirWeb.Plug.Redirect,
    rules: %{
      "/docs" => "/docs/"
    }
  )

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug(
    Plug.Static,
    at: "/",
    from: :air,
    gzip: false,
    only: ~w(frontend docs)
  )

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket("/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket)
    plug(Phoenix.LiveReloader)
    plug(Phoenix.CodeReloader)
  end

  plug(Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"
  )

  plug(Plug.RequestId)
  plug(Plug.Logger, log: :debug)

  plug(
    Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Jason
  )

  plug(Plug.MethodOverride)
  plug(Plug.Head)

  plug(Plug.Session, @session_options)

  # As per the Plug.Conn documentation, the remote_ip parameter is not automatically set
  # but should instead be set manually by a plug.
  plug(RemoteIp)

  plug(AirWeb.Router)
end
