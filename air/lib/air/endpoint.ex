defmodule Air.Endpoint do
  @moduledoc "Implements the HTTP server for insights.aircloak.com."

  use Phoenix.Endpoint, otp_app: :air
  use Aircloak.ChildSpec.Supervisor

  # bug in the current Phoenix -> should be fixed with the next version
  @dialyzer :no_unused


  # -------------------------------------------------------------------
  # Endpoint HTTP specification
  # -------------------------------------------------------------------

  socket "/cloak/socket", Air.Socket.Cloak
  socket "/frontend/socket", Air.Socket.Frontend

  plug Air.Plug.Rewrite, rules: %{
    ["docs"] => ["docs", "index.html"]
  }

  plug Air.Plug.Redirect, rules: %{
    "/docs" => "/docs/"
  }

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/", from: :air, gzip: false,
    only: ~w(css fonts images js robots.txt docs)

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Logger, log: :debug

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  plug Plug.Session,
    store: :cookie,
    key: "_air_key",
    signing_salt: "hkTRmL2h"

  # As per the Plug.Conn documentation, the remote_ip parameter is not automatically set
  # but should instead be set manually by a plug.
  plug RemoteIp

  plug Air.Router
end
