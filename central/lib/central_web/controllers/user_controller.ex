defmodule CentralWeb.UserController do
  @moduledoc false
  use Central.Web, :controller

  alias Central.{Schemas, Service}

  plug :load_user when action in [:edit, :update, :delete]


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html", users: Service.User.all())
  end

  def new(conn, _params) do
    render(conn, "new.html", changeset: Schemas.User.empty_changeset())
  end

  def edit(conn, _params) do
    user = conn.assigns.user
    render(conn, "edit.html", changeset: Schemas.User.changeset(user), user: user)
  end

  def create(conn, params) do
    case Service.User.create(params["user"]) do
      {:ok, _user} ->
        conn
        |> put_flash(:info, "User created")
        |> redirect(to: user_path(conn, :index))
      {:error, changeset} -> render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, params) do
    case Service.User.update(conn.assigns.user, params["user"]) do
      {:ok, _user} ->
        conn
        |> put_flash(:info, "User updated")
        |> redirect(to: user_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def delete(conn, _params) do
    case Service.User.delete(conn.assigns.user) do
      :ok ->
        conn
        |> put_flash(:info, "User deleted")
        |> redirect(to: user_path(conn, :index))
      :error ->
        conn
        |> put_flash(:error, "Could not delete the user")
        |> redirect(to: user_path(conn, :index))
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_user(conn, _) do
    case Service.User.get(conn.params["id"]) do
      {:error, :not_found} -> not_found(conn)
      {:ok, user} -> assign(conn, :user, user)
    end
  end
end
