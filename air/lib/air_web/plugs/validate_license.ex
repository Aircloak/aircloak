defmodule AirWeb.Plug.ValidateLicense do
  @moduledoc """
  This plug terminates the request if the Aircloak installation does not have a valid license. It shows the license
  page for admins and a generic notice about license missing for other users.
  """

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  if Mix.env() != :test do
    @impl Plug
    def call(conn, _opts) do
      cond do
        Air.Service.License.valid?() -> conn
        Air.Schemas.User.admin?(conn.assigns.current_user) and not json?(conn)->
          conn
          |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.admin_license_path(conn, :edit))
          |> Plug.Conn.halt()
        true ->
          conn
          |> Plug.Conn.put_status(Plug.Conn.Status.code(:payment_required))
          |> Phoenix.Controller.render(AirWeb.LicenseInvalidView, :invalid, layout: false)
          |> Plug.Conn.halt()
      end
    end

    defp json?(conn) do
      conn
      |> Plug.Conn.get_req_header("content-type")
      |> Enum.any?(& &1 |> String.downcase() |> String.contains?("application/json"))
    end
  else
    @impl Plug
    def call(conn, _opts), do: conn
  end
end
