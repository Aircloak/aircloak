defmodule Air.Endpoint do
  @moduledoc false
  use Phoenix.Endpoint, otp_app: :air

  # bug in the current Phoenix -> should be fixed with the next version
  @dialyzer :no_unused

  @doc """
    Adapts the endpoint configuration to runtime conditions.
    This is basically needed to support running multiple instances of the app on the dev machine.
  """
  def configure do
    endpoint_config = Application.get_env(:air, Air.Endpoint, [])
    port_offset = String.to_integer(System.get_env("INSIGHTS_PORT_OFFSET") || "0")
    if port_offset > 0 do
      runtime_endpoint_config = update_in(endpoint_config, [:http, :port],
          fn(port_base) -> port_base + port_offset end)
      Application.put_env(:air, Air.Endpoint, runtime_endpoint_config)
    end
  end

  socket "/socket", Air.UserSocket

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/", from: :air, gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)

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
