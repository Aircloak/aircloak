defmodule AirWeb.PrivacyPolicyController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Service.User

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def accept(conn, _params) do
    user = conn.assigns.current_user
    User.accept_privacy_policy(user)

    conn
    |> put_flash(:info, "The privacy policy has been accepted.")
    |> redirect(to: "/")
  end

  def reject(conn, _params) do
    user = conn.assigns.current_user
    User.reject_privacy_policy(user)

    conn
    |> put_flash(:error, "The privacy policy has been rejected. You cannot use Aircloak Insights.")
    |> redirect(to: "/")
  end
end
