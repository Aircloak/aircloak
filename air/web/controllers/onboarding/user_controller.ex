defmodule Air.Onboarding.UserController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{User, Group, AuditLog}


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
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
      changeset = User.changeset(%User{})
      render(conn, "new.html", changeset: changeset)
    end
  end

  def already_setup(conn, _params) do
    render(conn, "already_setup.html")
  end

  def create(conn, params) do
    changeset = User.new_user_changeset(%User{}, params["user"])
    case params["user"]["master_password"] == "bubblegum" do
      true ->
        group = get_admin_group()
        changeset = User.changeset(changeset, %{groups: [group.id]})
        case Repo.insert(changeset) do
          {:ok, user} ->
            AuditLog.log(conn, "Created onboarding admin user", user: user.email, name: user.name)
            login(conn, params["user"])
          {:error, changeset} -> render(conn, "new.html", changeset: changeset)
        end
      false ->
        changeset =  Ecto.Changeset.add_error(changeset, :master_password, "The master password is incorrect")
        # We need to trick add the action being performed, to get the form to render errors
        changeset = %{changeset | action: :insert}
        render(conn, "new.html", changeset: changeset)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp get_admin_group() do
    case Repo.all(from g in Group, where: g.admin) do
      [] -> create_admin_group()
      [group | _] -> group
    end
  end

  defp create_admin_group() do
    params = %{
      name: "Admin",
      admin: true,
    }
    %Group{}
    |> Group.changeset(params)
    |> Repo.insert!()
  end

  defp login(conn, params) do
    login_params = Map.take(params, ["email", "password"])
    conn = put_session(conn, :return_path, admin_user_path(conn, :index))
    Air.SessionController.create(conn, login_params)
  end
end
