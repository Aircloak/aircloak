defmodule Mix.Tasks.Compile.Guides do
  @shortdoc "Compiles user guides into HTML structure."
  @moduledoc "Compiles user guides into HTML structure."
  use Mix.Task
  require EEx

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    File.rm_rf("_build/guides")
    File.mkdir_p!("_build/guides")
    File.write!("_build/guides/installation.html", content())
    File.cp_r!("source/static", "_build/guides/static")
    Mix.Shell.IO.info("Compiled user guides can be found in the `_build/guides` folder.")
  end

  EEx.function_from_string :defp, :content,
    """
      <html>
        <head>
          <script src="static/js/clipboard.min.js"></script>
          <script src="static/js/app.js"></script>
          <link rel="stylesheet" href="static/css/app.css">
        </head>
        <body>
          <div class="content">
            <%= guide_content() %>
          </div>
        </body>
      </html>
    """,
    []

  defp guide_content(), do:
    Earmark.as_html!(File.read!("source/installation.md"))
end
