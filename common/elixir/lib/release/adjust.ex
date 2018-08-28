defmodule Aircloak.Release.Adjust do
  @moduledoc """
  Distillery plugin which allows adjusting files and folders which remain in the release.

  This plugin can be used to finalize the shape of the `priv` folder, and keep only the required files in the release.

  Example:

  ```
  # in release config.exs

  environment :local do
    plugin(
      Aircloak.Release.Adjust,

      # in app1, folder_a, only the listed files will be kept
      keep_only: [app1: [{folder_a, ~w(file_b folder_c)}, ...]],


      # in app1, folder_d, the listed files will be removed
      remove: [app1: [{folder_d, ~w(file_e file_f)}, ...]],

      ...
    )
  end
  ```
  """

  use Mix.Releases.Plugin

  # Mix.Releases.Plugin behaviour is not in PLT since dialyzer is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  # -------------------------------------------------------------------
  # Mix.Releases.Plugin callbacks
  # -------------------------------------------------------------------

  @impl Mix.Releases.Plugin
  def before_assembly(%Release{} = release, _opts) do
    release
  end

  @impl Mix.Releases.Plugin
  def after_assembly(%Release{} = release, instructions) do
    Enum.each(instructions, &interpret_instruction(release, &1))
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

  defp interpret_instruction(release, {:keep_only, list}) do
    for {app, folders} <- list,
        {folder, keep} <- folders,
        do: keep_only(release, app, to_string(folder), MapSet.new(keep))
  end

  defp interpret_instruction(release, {:remove, list}) do
    for {app, folders} <- list,
        {parent, removes} <- folders,
        remove <- removes,
        do: File.rm_rf(app_dir(release, app, Path.join(parent, remove)))
  end

  def keep_only(release, app, folder, keep) do
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

  defp app_vsn(release, app),
    do: to_string(Enum.find(release.applications, &(&1.name == app)).vsn)
end
