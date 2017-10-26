defmodule AirWeb.Onboarding.UserController do
  @moduledoc false
  use Air.Web, :controller
  alias Air.Service.User


  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      anonymous: [:new, :create, :already_setup]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, _params) do
    if User.admin_user_exists?() do
      redirect(conn, to: onboarding_user_path(conn, :already_setup))
    else
      render(conn, "new.html", changeset: User.empty_changeset(), errors: false)
    end
  end

  def already_setup(conn, _params) do
    render(conn, "already_setup.html")
  end

  def create(conn, params) do
    case User.create_onboarding_admin_user(params) do
      {:ok, user} ->
        audit_log(conn, "Created onboarding admin user", user: user.email, name: user.name)
        login(conn, params["user"])
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset, errors: true)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp login(conn, params) do
    login_params = Map.take(params, ["email", "password"])
    conn = put_session(conn, :return_path, admin_user_path(conn, :index))
    AirWeb.SessionController.create(conn, login_params)
  end
end
