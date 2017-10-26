defmodule CentralWeb.Endpoint do
  @moduledoc "Implements the HTTP server for insights.aircloak.com."

  use Aircloak.ChildSpec.Supervisor, start: {__MODULE__, :start_site, []}

  use Phoenix.Endpoint, otp_app: :central
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the site supervision tree."
  @spec start_site() :: Supervisor.on_start
  def start_site() do
    reset_air_statuses()
    start_link()
  end


  # -------------------------------------------------------------------
  # Endpoint HTTP specification
  # -------------------------------------------------------------------

  socket "/air/socket", CentralWeb.Socket.Air
  socket "/frontend/socket", CentralWeb.Socket.Frontend

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/", from: :central, gzip: false,
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
    parsers: [Central.Plug.KibanaProxyParser, :urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  plug Plug.Session,
    store: :cookie,
    key: "_central_key",
    signing_salt: "hkTRmL2h"

  plug CentralWeb.Router


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp reset_air_statuses() do
    try do
      Central.Service.Customer.reset_air_statuses()
    catch type, error ->
      Logger.error([
        "Error resetting air statuses: #{inspect(type)}:#{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])
    end
  end
end
