defmodule Air.Plug.Expiration do
  @moduledoc """
  This plug terminates the request with a redirect to the upgrade page
  if the Aircloak installation runs an expired version.
  """

  @behaviour Plug

  alias Air.Service.Version

  import Plug.Conn

  @doc false
  def init(opts), do: opts

  @doc false
  def call(conn, _opts) do
    if Version.expired?() do
      conn
      |> put_status(Plug.Conn.Status.code(:upgrade_required))
      |> Phoenix.Controller.render(Air.OutOfDateView, :expired, layout: false)
      |> halt()
    else
      conn
    end
  end
end
