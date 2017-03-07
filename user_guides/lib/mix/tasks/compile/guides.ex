defmodule Mix.Tasks.Compile.Guides do
  @shortdoc "Compiles user guides into HTML structure."
  @moduledoc "Compiles user guides into HTML structure."
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    File.mkdir_p!("html")
    File.rm_rf!("html/*")
    File.write!("html/installation.html", content())
    Mix.Shell.IO.info("Compiled user guides can be found in the `html` folder.")
  end

  defp content(), do:
    Earmark.as_html!(File.read!("guides/installation.md"))
end
