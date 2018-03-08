defmodule AirWeb.Plug.ValidateLicense do
  @moduledoc """
  This plug terminates the request if the Aircloak installation does not have a valid license. It shows the license
  page for admins and a generic notice about license missing for other users.
  """

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  if Mix.env() != :test do
    import Plug.Conn

    alias Air.Service.License

    @impl Plug
    def call(conn, _opts) do
      if License.valid?() do
        conn
      else
        conn
        |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.admin_license_invalid_path(conn, :invalid))
        |> halt()
      end
    end
  else
    @impl Plug
    def call(conn, _opts), do: conn
  end
end
