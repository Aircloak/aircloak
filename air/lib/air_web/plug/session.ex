defmodule AirWeb.Plug.Session do
  @moduledoc "Base module for plugs dealing with authentication."

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @session_key "_air_session_token"
  @max_session_age_seconds div(30 * :timer.hours(24), :timer.seconds(1))

  alias Air.Service.RevokableToken
  alias Air.Schemas.User

  @doc "Returns the session token stored in the given connection."
  @spec get(Plug.Conn.t()) :: String.t() | nil
  def get(conn), do: Plug.Conn.get_session(conn, @session_key)

  @doc "Replaces the session token stored in the given connection."
  @spec put(Plug.Conn.t(), String.t()) :: Plug.Conn.t()
  def put(conn, token), do: Plug.Conn.put_session(conn, @session_key, token)

  @doc "Creates a new session token for the user and sets it in the given conn."
  @spec sign_in(Plug.Conn.t(), Air.Schemas.User.t()) :: Plug.Conn.t()
  def sign_in(conn, user) do
    Plug.Conn.put_session(conn, @session_key, RevokableToken.sign(user.id, user, :session, @max_session_age_seconds))
  end

  @doc "Revokes the session token in the given conn."
  @spec sign_out(Plug.Conn.t()) :: Plug.Conn.t()
  def sign_out(conn) do
    conn |> Plug.Conn.get_session(@session_key) |> RevokableToken.revoke(:session)
    Plug.Conn.delete_session(conn, @session_key)
  end

  @doc "Returns the current session token."
  @spec current_token(Plug.Conn.t()) :: String.t()
  def current_token(conn), do: Plug.Conn.get_session(conn, @session_key)

  @doc "Returns true if the connection is authenticated (as any user), false otherwise."
  @spec authenticated?(Plug.Conn.t()) :: boolean()
  def authenticated?(conn), do: not is_nil(conn.assigns.current_user)

  @doc "Returns {:ok, user} if the conn contains a valid session, :error otherwise."
  @spec load_user(Plug.Conn.t()) :: {:ok, User.t()} | :error
  def load_user(conn) do
    with {:ok, user_id} <- unpack_session(conn),
         {:ok, user} <- Air.Service.User.load_enabled(user_id) do
      {:ok, user}
    else
      _ -> :error
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp unpack_session(conn), do: conn |> get() |> Air.Service.RevokableToken.verify(:session)
end
