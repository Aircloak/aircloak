defmodule Air.Plug.Rewrite do
  @moduledoc """
  Rewrites incoming path to a different path.

  This plug allows you to provide user-friendly paths, which will be rewritten
  to some internal path. This makes it possible to process some non-standard path
  by other plugs in the endpoint.

  For example, let's say we have a static file accessible at `/api_docs/index.html`
  and we want to expose the access through `/api_docs`. It can be done as:

  ```elixir
  plug Air.Plug.Rewrite, rules: %{
    ["api_docs"] => ["api_docs", "index.html"]
  }

  plug Plug.Static,
    ...
  ```

  The rewrite mapping will transform `/api_docs` request into `/api_docs/index.html`.
  This will then be picked up by the subsequent `Plug.Static` and serve the proper
  content.
  """
  @behaviour Plug
  alias Plug.Conn


  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(opts), do: opts

  @doc false
  def call(conn, opts) do
    case Map.fetch(opts[:rules], conn.path_info) do
      :error -> conn
      {:ok, transformed_path} -> %Conn{conn | path_info: transformed_path}
    end
  end
end
