defmodule Air.Plug.Redirect do
  @moduledoc """
  Redirects incoming requests at one path to another.

  For example, let's say we want to ensure that a user visiting docs
  should be redirected to docs/, in order to ensure that relative
  assets are loaded correctly. This could be done as:

  ```elixir
  plug Air.Plug.Redirect, rules: %{
    "docs" => "docs/"
  }

  plug Plug.Static,
    ...
  ```
  """

  @behaviour Plug


  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, opts) do
    case Map.fetch(opts[:rules], conn.request_path) do
      :error -> conn
      {:ok, transformed_path} ->
        Phoenix.Controller.redirect(conn, to: transformed_path)
    end
  end
end
