defmodule Central.SessionController do
  @moduledoc false
  use Central.Web, :controller


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, params) do
    case Central.Service.User.login(params["email"], params["password"]) do
      {:ok, user} ->
        return_path = get_session(conn, :return_path) || "/"
        conn
        |> Guardian.Plug.sign_in(user)
        |> conditionally_create_persistent_login(params)
        |> put_session(:return_path, nil)
        |> put_flash(:info, "Logged in successfully. Welcome back!")
        |> redirect(to: return_path)
      _ ->
        conn
        |> put_flash(:error, "Invalid e-mail or password.")
        |> render("new.html")
    end
  end

  def delete(conn, _params) do
    conn
    |> Guardian.Plug.sign_out()
    |> Central.Plug.Session.Restoration.remove_token()
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: session_path(conn, :new))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conditionally_create_persistent_login(conn, %{"remember" => "on"}) do
    Central.Plug.Session.Restoration.persist_token(conn)
  end
  defp conditionally_create_persistent_login(conn, _params), do: conn
end
