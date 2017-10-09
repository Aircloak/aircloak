defmodule Mix.Tasks.SiteRelease do
  @shortdoc "Builds the full release, including static digests"
  @moduledoc """
  Builds the full release, including static digests.

  Usage:

  ```
  mix site_release
  ```
  """
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @impl Mix.Task
  def run(args) do
    {:ok, cwd} = File.cwd()
    brunch_bin = Path.join([cwd, "node_modules/brunch/bin/brunch"])
    {_, 0} = System.cmd(brunch_bin, ["build", "--production"])
    :ok = Mix.Task.run("phoenix.digest")
    :ok = Mix.Task.run("release", args)
  end
end
