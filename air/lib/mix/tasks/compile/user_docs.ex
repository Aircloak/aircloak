defmodule Mix.Tasks.Compile.UserDocs do
  @shortdoc "Compiles the API documentation."
  @moduledoc "Compiles the API documentation."
  use Mix.Task
  require Logger

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    if stale?() do
      cmd!("yarn", ~w(install))
      conditionally_compile_offline_docs()
      cmd!("yarn", ~w(run gitbook build))
      File.mkdir_p!("priv/static")
      File.rm_rf!("priv/static/docs")
      File.cp_r!("docs/_book", "priv/static/docs")
      Mix.Shell.IO.info("Compiled user docs")
    end
  end

  defp stale?() do
    try do
      source_mtime =
        Path.wildcard("docs/**")
        |> Enum.reject(& &1 =~ ~r/aircloak-docs/)
        |> Enum.map(&File.stat!(&1).mtime)
        |> Enum.max()

      Path.wildcard("priv/static/docs/**")
      |> Enum.reject(& &1 =~ ~r/aircloak-docs/)
      |> Enum.map(&File.stat!(&1).mtime)
      |> Enum.sort(&(&1 > &2))
      |> case do
        [] -> true
        [target_mtime | _] -> target_mtime < source_mtime
      end
    catch
      # For some unknown reason, File.stat! fails on a local docker build. Since it is not critical,
      # here we're just suppressing the error and assuming that the target is stale.
      _,_ -> true
    end
  end

  defp cmd!(cmd, args) do
    case System.cmd(cmd, args,
          stderr_to_stdout: true,
          into: IO.stream(:stdio, :line),
          cd: "docs"
        ) do
      {_, 0} -> :ok
      {_, _} ->
        Mix.raise("Error building user docs")
    end
  end

  defp conditionally_compile_offline_docs() do
    if has_ebook_convert_installed() do
      # Use a readme that doesn't contain links to the offline content
      cmd!("ln", ~w(-sf README-offline.md content/README.md))
      # Ignore offline assets so we don't recursively bundle the offline assets in themselves
      cmd!("ln", ~w(-sf offline-doc-ignores content/.bookignore))
      try do
        cmd!("yarn", ~w(run gitbook pdf ./ content/aircloak-docs.pdf))
        cmd!("yarn", ~w(run gitbook epub ./ content/aircloak-docs.epub))
        cmd!("yarn", ~w(run gitbook mobi ./ content/aircloak-docs.mobi))
      after
        cmd!("ln", ~w(-sf README-online.md content/README.md))
        cmd!("rm", ~w(-f content/.bookignore))
      end
    end
  end

  defp has_ebook_convert_installed() do
    try do
      case System.cmd("ebook-convert", ~w(--version)) do
        {_, 0} -> true
        {_, _} -> false
      end
    rescue _ -> false
    end
  end
end
