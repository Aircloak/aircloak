defmodule AirWeb.Plug.ValidateLicense.Browser do
  @moduledoc """
  This plug terminates the request if the Aircloak installation does not have a valid license. It shows the license
  page for admins and a generic notice about license missing for other users.
  """

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    cond do
      Air.Service.License.valid?() ->
        conn

      Air.Schemas.User.admin?(conn.assigns.current_user) ->
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
end

defmodule AirWeb.Plug.ValidateLicense.API do
  @moduledoc """
  This plug terminates the request if the Aircloak installation does not have a valid license. It always shows a generic
  notice, so is suitable for use in the API pipeline.
  """

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    if Air.Service.License.valid?() do
      conn
    else
      conn
      |> Plug.Conn.put_status(Plug.Conn.Status.code(:payment_required))
      |> Phoenix.Controller.render(AirWeb.LicenseInvalidView, :invalid, layout: false)
      |> Plug.Conn.halt()
    end
  end
end
