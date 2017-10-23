defmodule Air.MonitoringEndpoint do
  @moduledoc "Implements the HTTP server for serving monitoring data."

  use Phoenix.Endpoint, otp_app: :air
  use Aircloak.ChildSpec.Supervisor


  # -------------------------------------------------------------------
  # Endpoint HTTP specification
  # -------------------------------------------------------------------

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

  plug Air.MonitoringRouter
end
