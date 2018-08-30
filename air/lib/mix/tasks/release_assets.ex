defmodule Mix.Tasks.ReleaseAssets do
  @shortdoc "Builds the assets for release."
  @moduledoc "Builds the assets for release."
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @impl Mix.Task
  def run(_args) do
    {:ok, cwd} = File.cwd()
    brunch_bin = Path.join([cwd, "assets/node_modules/brunch/bin/brunch"])
    {_, 0} = System.cmd(brunch_bin, ["build", "--production"], cd: "assets")
  end
end
