defmodule CentralWeb.Plug.Rewrite do
  @moduledoc """
  Rewrites incoming path to a different path.

  This plug allows you to provide user-friendly paths, which will be rewritten
  to some internal path. This makes it possible to process some non-standard path
  by other plugs in the endpoint.

  For example, let's say we have a static file accessible at `/docs/index.html`
  and we want to expose the access through `/docs`. It can be done as:

  ```elixir
  plug Central.Plug.Rewrite, rules: %{
    ["docs"] => ["docs", "index.html"]
  }

  plug Plug.Static,
    ...
  ```

  The rewrite mapping will transform `/docs` request into `/docs/index.html`.
  This will then be picked up by the subsequent `Plug.Static` and serve the proper
  content.
  """
  @behaviour Plug
  alias Plug.Conn


  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, opts) do
    case Map.fetch(opts[:rules], conn.path_info) do
      :error -> conn
      {:ok, transformed_path} -> %Conn{conn | path_info: transformed_path}
    end
  end
end
