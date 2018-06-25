defmodule Air.TestConnHelper do
  @moduledoc "Helpers for working with the connection."

  alias AirWeb.Endpoint
  import Phoenix.ConnTest

  @doc "Logs-in as the given user, and returns the recycled connection."
  defmacro login(user) do
    quote do
      build_conn()
      |> post("/auth", email: unquote(user).email, password: "password1234")
      |> recycle()
    end
  end

  @doc "Simulates a connection authenticated with the given token"
  defmacro api_conn(token) do
    quote do
      token_salt = Application.get_env(:air, Endpoint) |> Keyword.fetch!(:api_token_salt)
      token_text = Phoenix.Token.sign(Endpoint, token_salt, unquote(token).id)

      put_req_header(build_conn(), "auth-token", token_text)
    end
  end
end
