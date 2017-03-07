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

  defp content(), do:
    "source/installation.md"
    |> File.read!()
    |> Earmark.as_html!()
    |> layout()

  EEx.function_from_string :defp, :layout, File.read!("source/layout.html.eex"), [:content]
end
