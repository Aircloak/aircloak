defmodule Air.TestConnHelper do
  @moduledoc "Helpers for working with the connection."

  import Phoenix.ConnTest

  @doc "Logs-in as the given user, and returns the recycled connection."
  defmacro login(user) do
    quote do
      conn()
      |> post("/auth", email: unquote(user).email, password: "1234")
      |> recycle()
    end
  end
end
