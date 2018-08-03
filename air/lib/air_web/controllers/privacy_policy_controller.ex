defmodule AirWeb.PrivacyPolicyController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Service.PrivacyPolicy

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      anonymous: [:index],
      user: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    case PrivacyPolicy.get() do
      {:ok, privacy_policy} -> render(conn, "index.html", privacy_policy: privacy_policy)
      {:error, :no_privacy_policy_created} -> render(conn, "missing.html")
    end
  end
end
