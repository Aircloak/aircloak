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

  @doc false
  def run(_args) do
    File.rm_rf!("rel/air")
    {_, 0} = System.cmd("brunch", ["build", "--production"])
    :ok = Mix.Task.run("phoenix.digest")
    :ok = Mix.Task.run("release", ["--no-confirm-missing"])
  end
end
