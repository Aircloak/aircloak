defmodule Air.MonitoringEndpoint do
  @moduledoc "Implements the HTTP server for serving monitoring data."

  use Phoenix.Endpoint, otp_app: :air

  # bug in the current Phoenix -> should be fixed with the next version
  @dialyzer :no_unused


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

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do:
    %{
      id: __MODULE__, restart: :permanent, shutdown: :infinity, type: :supervisor,
      start: {__MODULE__, :start_link, []},
    }
end
