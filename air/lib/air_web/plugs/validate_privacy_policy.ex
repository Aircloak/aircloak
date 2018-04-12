defmodule AirWeb.Plug.ValidatePrivacyPolicy.Browser do
  @moduledoc """
  This plug terminates the request if the Aircloak installation does not yet have a valid privacy policy.

  Admin users are redirected to a page where they can create one, whereas normal
  users are told to await an administrative user creating a policy.
  """

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    if Air.Service.PrivacyPolicy.exists?() do
      conn
    else
      if Air.Schemas.User.admin?(conn.assigns.current_user) do
        conn
        |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.admin_privacy_policy_path(conn, :new))
        |> Plug.Conn.halt()
      else
        conn
        |> Plug.Conn.put_status(Plug.Conn.Status.code(:precondition_failed))
        |> Phoenix.Controller.render(AirWeb.PrivacyPolicyMissingView, :missing, layout: false)
        |> Plug.Conn.halt()
      end
    end
  end
end

defmodule AirWeb.Plug.ValidatePrivacyPolicy.API do
  @moduledoc """
  This plug terminates the request if the Aircloak installation does not yet have a valid privacy policy.
  It always shows a generic notice, so is suitable for use in the API pipeline.
  """

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    if Air.Service.PrivacyPolicy.exists?() do
      conn
    else
      conn
      |> Plug.Conn.put_status(Plug.Conn.Status.code(:precondition_failed))
      |> Phoenix.Controller.render(AirWeb.PrivacyPolicyMissingView, :missing, layout: false)
      |> Plug.Conn.halt()
    end
  end
end
