defmodule Air.Plugs.GuardianSessionRestoration do
  @moduledoc """
  Plug that allows us to provide a "remember me" feature in our login system.
  The guardian authentication system checks the session store for the authentication token.
  When a user wants us to remember that he is logged in, we create an additional cookie
  that allows us to restore the session variable on subsequent visits.
  """
  require Logger

  @cookie_key "auth_remember_me"
  # 30 days in seconds (30*24*60*60) - the time before a user has to login again.
  @cookie_max_age_s 2592000


  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  def init(default), do: default

  def call(conn, _default) do
    case Plug.Conn.get_session(conn, session_key()) do
      nil ->
        conditionally_restore_session(conn)
      _ ->
        # A session already exists, so we don't need to do anything at all
        conn
    end
  end


  # -------------------------------------------------------------------
  # Utility functions
  # -------------------------------------------------------------------

  def persist_token(conn) do
    Logger.debug("The user wants us to remember that s/he is logged in")
    jwt = Plug.Conn.get_session(conn, session_key())
    Plug.Conn.put_resp_cookie(conn, @cookie_key, jwt, max_age: @cookie_max_age_s)
  end

  def remove_token(conn) do
    Logger.debug("The user wants us to forget that s/he was logged in")
    Plug.Conn.delete_resp_cookie(conn, @cookie_key, max_age: @cookie_max_age_s)
  end

  defp conditionally_restore_session(conn) do
    %Plug.Conn{req_cookies: req_cookies} = Plug.Conn.fetch_cookies(conn)
    case req_cookies[@cookie_key] do
      nil ->
        # The user isn't logged in, or didn't use the remember-me feature
        conn
      jwt ->
        Logger.debug("Restoring user session from cookie, logging in the user")
        Plug.Conn.put_session(conn, session_key(), jwt)
    end
  end

  defp session_key() do
    Guardian.Keys.base_key(:default)
  end
end
