defmodule Mix.Tasks.Compile.ApiDocs do
  @shortdoc "Compiles the API documentation."
  @moduledoc "Compiles the API documentation."
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    if stale?() do
      cmd!("bundle", ~w(install --path vendor/bundle))
      cmd!("bundle", ~w(exec middleman build))
      File.mkdir_p!("priv/static")
      File.rm_rf!("priv/static/api_docs")
      File.cp_r!("api_docs/build", "priv/static/api_docs")
      Mix.Shell.IO.info("Compiled api_docs")
    end
  end

  defp stale?() do
    source_mtime =
      Path.wildcard("api_docs/**")
      |> Enum.map(&File.stat!(&1).mtime)
      |> Enum.max()

    Path.wildcard("priv/static/api_docs/**")
    |> Enum.map(&File.stat!(&1).mtime)
    |> Enum.sort(&(&1 > &2))
    |> case do
      [] -> true
      [target_mtime | _] -> target_mtime < source_mtime
    end
  end

  defp cmd!(cmd, args) do
    case System.cmd(cmd, args,
          stderr_to_stdout: true,
          into: IO.stream(:stdio, :line),
          cd: "api_docs"
        ) do
      {_, 0} -> :ok
      {_, _} ->
        Mix.raise("Error building api docs")
    end
  end
end
