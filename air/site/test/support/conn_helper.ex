defmodule Air.TestConnHelper do
  @moduledoc "Helpers for working with the connection."

  alias Air.Endpoint
  import Phoenix.ConnTest

  @doc "Logs-in as the given user, and returns the recycled connection."
  defmacro login(user) do
    quote do
      conn()
      |> post("/auth", email: unquote(user).email, password: "1234")
      |> recycle()
    end
  end

  @doc "Simulates a connection authenticated with the given token"
  defmacro api_conn(token) do
    quote do
      token_salt = Application.get_env(:air, Endpoint) |> Keyword.fetch!(:api_token_secret)
      token_text = Phoenix.Token.sign(Endpoint, token_salt, unquote(token).id)

      put_req_header(conn(), "auth-token", token_text)
    end
  end
end
