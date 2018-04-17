defmodule AirWeb.PrivacyPolicyController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Service.{User, PrivacyPolicy}

  plug(:load_privacy_policy when action in [:accept, :reject])

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
    {:ok, privacy_policy} = PrivacyPolicy.get()
    render(conn, "index.html", privacy_policy: privacy_policy)
  end

  def accept(conn, _params) do
    User.accept_privacy_policy!(conn.assigns.current_user, conn.assigns.privacy_policy)

    conn
    |> put_flash(:info, "The privacy policy has been accepted.")
    |> redirect(to: "/")
  end

  def reject(conn, _params) do
    User.reject_privacy_policy!(conn.assigns.current_user)

    conn
    |> put_flash(:error, "The privacy policy has been rejected. You cannot use Aircloak Insights.")
    |> redirect(to: "/")
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  defp load_privacy_policy(conn, _params) do
    id = conn.params["id"]

    case PrivacyPolicy.get_by_id(id) do
      {:ok, privacy_policy} ->
        assign(conn, :privacy_policy, privacy_policy)

      {:error, _} ->
        conn
        |> put_flash(:error, "Specific privacy policy not found. Please try again.")
        |> redirect(to: privacy_policy_path(conn, :index))
        |> halt()
    end
  end
end
