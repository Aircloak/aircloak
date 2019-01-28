defmodule AirWeb.Plug.Session.Restoration do
  @moduledoc """
  Plug that allows us to provide a "remember me" feature in our login system.  The guardian authentication system
  checks the session store for the authentication token.  When a user wants us to remember that he is logged in, we
  create an additional cookie that allows us to restore the session variable on subsequent visits. This implementation
  is currently adapted from https://github.com/ueberauth/guardian/blob/master/lib/guardian/plug/verify_cookie.ex.
  Once guardian 1.1.0 is released we should just use the implementation from the lib.
  """
  @behaviour Plug

  require Logger
  alias Air.Schemas.User

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

  def call(conn, opts) do
    with nil <- Guardian.Plug.current_token(conn, opts),
         {:ok, token} <- find_token_from_cookies(conn),
         active_session? <- Guardian.Plug.session_active?(conn),
         exchange_to <- Air.Guardian.default_token_type(),
         {:ok, _old, {new_token, new_claims}} <- Air.Guardian.exchange(token, "refresh", exchange_to) do
      conn
      |> Air.Guardian.Plug.put_current_token(new_token)
      |> Air.Guardian.Plug.put_current_claims(new_claims)
      |> maybe_put_in_session(active_session?, new_token, opts)
    else
      _ -> conn
    end
  end

  # -------------------------------------------------------------------
  # Utility functions
  # -------------------------------------------------------------------

  @doc "Persists the user session in the cookie."
  @spec persist_token(Plug.Conn.t(), User.t()) :: Plug.Conn.t()
  def persist_token(conn, user) do
    Logger.debug("The user wants us to remember that s/he is logged in")

    case Air.Guardian.encode_and_sign(user, _claims = %{}, token_type: "refresh") do
      {:ok, token, _new_claims} -> Plug.Conn.put_resp_cookie(conn, @cookie_key, token, max_age: @cookie_max_age_s)
      {:error, error} -> raise error
    end
  end

  @doc "Removes the persisted session from the cookie."
  @spec remove_token(Plug.Conn.t()) :: Plug.Conn.t()
  def remove_token(conn) do
    Logger.debug("The user wants us to forget that s/he was logged in")
    Plug.Conn.delete_resp_cookie(conn, @cookie_key, max_age: @cookie_max_age_s)
  end

  defp find_token_from_cookies(conn) do
    token = conn.req_cookies[@cookie_key]
    if token, do: {:ok, token}, else: :no_token_found
  end

  defp maybe_put_in_session(conn, false, _, _), do: conn

  defp maybe_put_in_session(conn, true, token, opts) do
    key = conn |> storage_key(opts) |> Guardian.Plug.Keys.token_key()
    Plug.Conn.put_session(conn, key, token)
  end

  defp storage_key(conn, opts), do: Guardian.Plug.Pipeline.fetch_key(conn, opts)
end
