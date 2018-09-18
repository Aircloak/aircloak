defmodule Mix.Tasks.Compile.Rodbc do
  @moduledoc "Rodbc compiler."

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    lib = "priv/native/rodbc"

    # We're doing a manual check if the lib needs to be recompiled. Otherwise, rustler always force compiles the
    # library. This leads to prolongated build times, especially during our docker builds, as the build artifacts
    # are always changed, and therefore the release build is always regenerated.
    if not File.exists?(lib) or File.stat!(lib).mtime < source_mtime() do
      Mix.Task.run("compile.rustler", args)
    else
      :ok
    end
  end

  defp source_mtime() do
    Path.wildcard("src/**")
    |> Enum.map(&File.stat!(&1).mtime)
    |> Enum.max()
  catch
    # For some unknown reason, File.stat! fails on a local docker build. Since it is not critical,
    # here we're just suppressing the error and assuming that the target is stale.
    _, _ ->
      true
  end
end
