defmodule AirWeb.DocsController do
  @moduledoc false
  use Air.Web, :controller

  plug(:put_layout, false)

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      anonymous: :all,
      user: :all,
      admin: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params), do: render(conn, "index.html")

  def sidebar(conn, _params),
    do: serve_transformed_markdown_file(conn, "docs_sidebar.md", &replace_version/1)

  def attacks_page(conn, _params),
    do: serve_transformed_markdown_file(conn, "attacks.md", &strip_github_links/1)

  def diffix_page(conn, _params),
    do: serve_transformed_markdown_file(conn, "diffix.md", &strip_github_links/1)

  def redirect(conn, _params) do
    if conn.path_info |> List.last() |> String.match?(~r/\.html$/i) do
      render(conn, "redirect.html")
    else
      not_found(conn)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp serve_transformed_markdown_file(conn, file_name, fun) do
    with {:ok, content} <- File.read(file_path(file_name)) do
      conn
      |> put_resp_content_type("text/markdown")
      |> send_resp(200, fun.(content))
      |> halt()
    else
      _ ->
        conn
        |> send_resp(500, "Could not serve this file.")
        |> halt()
    end
  end

  # Strips out markdown links of the type [aircloak/aircloak#XXXX](...)
  defp strip_github_links(content),
    do: String.replace(content, ~r/\[aircloak\/aircloak#\d+\]\(.+?\) ?/, "", global: true)

  defp replace_version(content),
    do: String.replace(content, "%VERSION%", Aircloak.Version.for_app(:air))

  defp file_path(file_name),
    do: Path.join([Application.app_dir(:air, "priv"), "static", file_name])
end
