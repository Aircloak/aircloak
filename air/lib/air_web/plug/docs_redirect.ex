defmodule AirWeb.Plug.DocsRedirect do
  @moduledoc """
  Sends a frontend redirector to convert old .html style
  paths to new docsify frontend based routes.
  """

  alias Plug.Conn
  @behaviour Plug

  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    conn.path_info
    |> Enum.map(&String.downcase/1)
    |> case do
      ["docs" | path] -> handle_docs_path(conn, path)
      _ -> conn
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp handle_docs_path(conn, ["index.html"]), do: conn
  defp handle_docs_path(conn, []), do: conn

  defp handle_docs_path(conn, path) do
    if path |> List.last() |> String.ends_with?(".html") do
      conn
      |> Conn.send_file(200, redirector_path())
      |> Conn.halt()
    else
      conn
    end
  end

  defp redirector_path(),
    do: Path.join([Application.app_dir(:air, "priv"), "static", "docs_redirect.html"])
end
