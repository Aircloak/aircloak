defmodule Air.TestConnHelper do
  @moduledoc "Helpers for working with the connection."

  @doc "Logs-in as the given user"
  @spec login(Plug.Conn.t, Air.User.t) :: Plug.Conn.t
  def login(conn, user) do
    conn
    |> init_session()
    |> Guardian.Plug.sign_in(user)
  end

  defp init_session(conn) do
    # Establishes the session, taken from
    # https://github.com/phoenixframework/phoenix/issues/861#issuecomment-100665548
    session_opts = Plug.Session.init([
          store: :cookie,
          key: "foobar",
          encryption_salt: "encrypted cookie salt",
          signing_salt: "signing salt",
          log: false,
          encrypt: false
        ])
    put_in(conn.secret_key_base, String.duplicate("abcdef0123456789", 8))
    |> Plug.Session.call(session_opts)
    |> Plug.Conn.fetch_session()
  end
end
