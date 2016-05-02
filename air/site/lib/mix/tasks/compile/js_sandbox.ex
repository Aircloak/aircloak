defmodule Mix.Tasks.Compile.JsSandbox do
  @shortdoc "Compiles JavaScript sandbox."
  @moduledoc "Compiles JavaScript sandbox."
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    system_cmd!("make", ["-C", "js_sandbox"])
    File.mkdir_p!("priv")
    File.cp_r!("js_sandbox/bin", "priv/js_sandbox/")
  end

  defp system_cmd!(cmd, args) do
    case System.cmd(cmd, args, stderr_to_stdout: true) do
      {_, 0} -> :ok
      {output, _} ->
        IO.puts "\n#{IO.ANSI.red()}#{output}#{IO.ANSI.reset()}"
        Mix.raise("Error running #{cmd}")
    end
  end
end
