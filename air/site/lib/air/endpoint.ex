defmodule Air.Endpoint do
  @moduledoc "Implements the HTTP server for insights.aircloak.com."

  use Phoenix.Endpoint, otp_app: :air

  # bug in the current Phoenix -> should be fixed with the next version
  @dialyzer :no_unused


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns the supervisor specification for this endpoint.

  The specification lists processes required to run this endpoint. This includes
  the endpoint process as well as auxiliary processes which register the endpoint
  to etcd.
  """
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec do
    import Supervisor.Spec, warn: false

    http_host = System.get_env("HTTP_HOST_IP") || "127.0.0.1"
    http_port = Application.get_env(:air, Air.Endpoint, [])[:http][:port]
    key = "/service_instances/insights/#{http_host}_#{http_port}"
    value = Poison.encode!(%{http_endpoint: "#{http_host}:#{http_port}"})

    children = [
      worker(Air.SyncRequester, [sync_requester()]),
      worker(__MODULE__, []),
      worker(Air.ServiceRegistration, [key, value], id: Air.Endpoint.ServiceRegistration)
    ]
    supervisor(Supervisor, [children, [strategy: :one_for_all]], id: Module.concat(__MODULE__, Supervisor))
  end

  @doc "Returns the name of the sync requester used by this endpoint"
  @spec sync_requester :: atom
  def sync_requester, do: __MODULE__


  # -------------------------------------------------------------------
  # Endpoint HTTP specification
  # -------------------------------------------------------------------

  socket "/cloak/socket", Air.Socket.Cloak

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/", from: :air, gzip: false,
    only: ~w(css fonts images js robots.txt)

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

  plug Air.Router
end
