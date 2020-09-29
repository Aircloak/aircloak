defmodule AirWeb.Plug.DiffixDocs do
  @moduledoc "Serves cleaned diffix docs."

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
      ["docs", "attacks.md"] -> serve(conn, "attacks.md")
      ["docs", "diffix.md"] -> serve(conn, "diffix.md")
      _ -> conn
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp serve(conn, file_name) do
    with {:ok, content} <- File.read(doc_path(file_name)) do
      conn
      |> Conn.put_resp_content_type("text/markdown")
      |> Conn.send_resp(200, strip_github_links(content))
      |> Conn.halt()
    else
      _ ->
        conn
        |> Conn.send_resp(500, "Could not serve this file.")
        |> Conn.halt()
    end
  end

  defp doc_path(file_name),
    do: Path.join([Application.app_dir(:air, "priv"), "static", file_name])

  # Strips out markdown links of the type [aircloak/aircloak#XXXX](...)
  defp strip_github_links(content),
    do: String.replace(content, ~r/\[aircloak\/aircloak#\d+\]\(.+?\) ?/, "", global: true)
end
