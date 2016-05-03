defmodule Mix.Tasks.Compile.JsSandbox do
  @shortdoc "Compiles JavaScript sandbox."
  @moduledoc "Compiles JavaScript sandbox."
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    case System.cmd("make", ["-C", "js_sandbox"],
          stderr_to_stdout: true,
          into: IO.stream(:stdio, :line)
        ) do
      {_, 0} -> :ok
      {_, _} ->
        Mix.raise("Error compiling js_sandbox")
    end
    File.mkdir_p!("priv")
    File.cp_r!("js_sandbox/bin", "priv/js_sandbox/")
    Mix.Shell.IO.info("Compiled js_sandbox")
  end
end
