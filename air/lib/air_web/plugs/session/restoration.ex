defmodule AirWeb.Plug.Session.Restoration do
  @moduledoc """
  Plug that allows us to provide a "remember me" feature in our login system. The regular session token is placed in a
  cookie with the lifetime of "session".  When a user wants us to remember that he is logged in, we create an additional
  cookie that allows us to restore the session token on subsequent visits.
  """
  @behaviour Plug

  require Logger
  alias AirWeb.Plug.Session
  alias Air.Service.Salts

  @cookie_key "auth_remember_me"
  # 30 days in seconds (30*24*60*60) - the time before a user has to login again.
  @cookie_max_age_s 2_592_000

  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @impl Plug
  def init(default), do: default

  @impl Plug
  def call(%{req_cookies: %Plug.Conn.Unfetched{}} = conn, opts) do
    conn
    |> Plug.Conn.fetch_cookies()
    |> call(opts)
  end

  def call(conn, _opts) do
    with {:ok, token} <- find_token_from_cookies(conn) do
      Session.put(conn, token)
    else
      _ -> conn
    end
  end

  # -------------------------------------------------------------------
  # Utility functions
  # -------------------------------------------------------------------

  @doc "Persists the current session in a longer-lived cookie."
  @spec persist_token(Plug.Conn.t()) :: Plug.Conn.t()
  def persist_token(conn) do
    Logger.debug("The user wants us to remember that they are logged in")

    token = Phoenix.Token.sign(AirWeb.Endpoint, Salts.get(:remember_me), Session.get(conn))
    Plug.Conn.put_resp_cookie(conn, @cookie_key, token, max_age: @cookie_max_age_s)
  end

  @doc "Removes the persisted session from the cookie."
  @spec remove_token(Plug.Conn.t()) :: Plug.Conn.t()
  def remove_token(conn) do
    Logger.debug("The user wants us to forget that they are logged in")
    Plug.Conn.delete_resp_cookie(conn, @cookie_key, max_age: @cookie_max_age_s)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp find_token_from_cookies(conn) do
    Phoenix.Token.verify(AirWeb.Endpoint, Salts.get(:remember_me), conn.req_cookies[@cookie_key],
      max_age: @cookie_max_age_s
    )
  end
end
