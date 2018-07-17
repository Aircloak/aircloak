defmodule Aircloak.Release.Whitelist do
  @moduledoc """
  Distillery plugin which allows whitelisting files and folders which remain in the release.

  This plugin can be used to finalize the shape of the `priv` folder, and keep only the required files in the release.

  Example:

  ```
  # in release config.exs

  environment :local do
    plugin(
      Aircloak.Release.Whitelist,

      # in app1, folder_a, only the listed files will be kept
      app1: [folder_a: ~w(file_b, folder_c)],

      ...
    )
  end
  ```
  """

  use Mix.Releases.Plugin

  # -------------------------------------------------------------------
  # Mix.Releases.Plugin callbacks
  # -------------------------------------------------------------------

  @impl Mix.Releases.Plugin
  def before_assembly(%Release{} = release, _opts) do
    release
  end

  @impl Mix.Releases.Plugin
  def after_assembly(%Release{} = release, whitelist) do
    for {app, folders} <- whitelist,
        {folder, keep} <- folders,
        do: whitelist(release, app, to_string(folder), MapSet.new(keep))

    release
  end

  @impl Mix.Releases.Plugin
  def before_package(%Release{} = release, _opts) do
    release
  end

  @impl Mix.Releases.Plugin
  def after_package(%Release{} = release, _opts) do
    release
  end

  @impl Mix.Releases.Plugin
  def after_cleanup(_args, _opts) do
    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def whitelist(release, app, folder, keep) do
    release
    |> existing_files(app, folder)
    |> Stream.reject(&MapSet.member?(keep, Path.basename(&1)))
    |> Enum.each(&File.rm_rf!/1)
  end

  defp existing_files(release, app, folder) do
    release
    |> app_dir(app, folder)
    |> Path.join("*")
    |> Path.wildcard()
  end

  defp app_dir(release, app, folder) do
    [release.output_dir, "lib", "#{app}-#{app_vsn(release, app)}", folder]
    |> Path.join()
    |> Path.expand()
  end

  defp app_vsn(release, app), do: to_string(Enum.find(release.applications, &(&1.name == app)).vsn)
end
